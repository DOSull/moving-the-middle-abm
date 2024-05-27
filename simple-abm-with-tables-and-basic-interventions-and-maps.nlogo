extensions [
  ;; matrix   ;; matrix maths
  csv      ;; easy reading of CSVs
  palette  ;; nicer colours
  table    ;; dictionary like tables for storing lots of the input parameters
  rnd
  gis
  profiler
]

globals [
  output-data-folder
  market-data-folder
  spatial-data-folder

  parcels-data
  luc-data

  farm-land
  show-labels?

  farm-types             ;; list of named farm types
  commodity-yield-means  ;; table of mean yields by LUC and farm-type
  commodity-yield-sds    ;; table of sd of yields by LUC and farm-type
  input-cost-means       ;; table of mean input costs by LUC and farm-type
  input-cost-sds         ;; table of sd of input costs by LUC and farm-type
  ghg-emission-means     ;; table of mean GHG emissions by LUC and farm-type
  ghg-emission-sds       ;; table of sd of GHG emissions by LUC and farm-type
  prices                 ;; table of commodity prices
  environmental-taxes    ;; table of additional environmental taxes/subsidies by farm-type
  base-thresholds-table  ;; table of default farmer decision thresholds for various interventions

  ;; colour settings - these can be changed in one place, see setup-key-colours procedure
  key-farmer-colour
  key-border-colour
  key-owner-link-colour
  key-LUC-palette
  key-LUC-palettes
  key-profit-colour
  key-loss-colour
  key-label-colour
  key-background-colour
]

breed [farmers farmer]
breed [farms farm]       ;; representative turtle for the farm
breed [interventions intervention]

farms-own [
  my-farmer              ;; the farmer who owns/runs this farm
  farm-type              ;; farm type of this farmer's farm
  the-land               ;; patch-set of the patches in this farm
  net-revenue            ;; net revenue of farm summed across patches
  my-interventions
  available-interventions
]

farmers-own [
  my-farm                ;; the farm turtle of this farmer's farm
;  thresholds-table       ;; lookup of probabilities of adoption of interventions by farm-type
]

;; guessing here what this might look like
interventions-own [
  intervention-type      ;; this would be one of the named types
  intervention-effects   ;; table of effects by farm-type for this intervention
]

patches-own [
  temp-ID
  the-owner              ;; the farmer who owns this patch
  luc-code               ;; LUC code where 1 = LUC1, 2 = LUC2, etc.
  ;; NOTE these are patch level parameter because they are set up with mean/sd and vary at patch level
  commodity-yields       ;; list of yields by farm type
  input-costs            ;; list of input costs by farm type
  ghg-emissions          ;; list of GHG emissions by farm type
]

;; -----------------------------------------
;; MAIN LOOP i.e., setup and go
;; -----------------------------------------
to setup
  clear-all
  reset-ticks

  let base-data-folder "data/"
  set output-data-folder word base-data-folder "output/"
  set market-data-folder word base-data-folder "market/"
  set spatial-data-folder (word base-data-folder "spatial/" region "/")

  set show-labels? true
  if seed-the-rng? [ random-seed rng-seed ]

  setup-farmer-parameters

  setup-key-colours

  ifelse setup-geography-from-files?
  [ setup-geography-from-files ]
  [ setup-random-geography ]
  display

  setup-economic-parameters
  go ;; this initialises the farms with current net revenue
end

;; the main model loop
to go
  if stop-model? [
    cleanup
    stop
  ]

  ;; much more action to go here...
  ask farmers [
    if any? [available-interventions] of my-farm [
      let potential-change consider-interventions
      if random-float 1 < last potential-change [
        print (word "Farmer " who " implementing " first potential-change " on " [farm-type] of my-farm " " [who] of my-farm)
        ask my-farm [
          implement-intervention first potential-change
        ]
      ]
    ]
  ]
  ask farms [
    update-net-revenue-of-farm
  ]

  tick
end

;; put a model stop condition here
to-report stop-model?
  report false
end

;; put any model clean up at end here
to cleanup
  ;; any end of run tidying up
end

;; -----------------------------------------
;; ALL THE SETUP STUFF
;; -----------------------------------------
;; farmer (behavioural) parameter setup
;; -----------------------------------------

;; we read the interventions by farm types file twice...
to setup-farmer-parameters
  ;; Assumes a CSV organised with farm types as column headings,
  ;; interventions as row names, i.e.
  ;;                  , SNB, Dairy, Forest, Crop
  ;; Build_Wetland    , 0.7, 0.75,  0.3,    0.5
  ;; Riparian_Planting, 0.7, 0.75,  0.2,    0.4
  ;; Clean_Races      , 0.2, 0.7,   0,      0
  ;; Farm_Plan        , 0.7, 0.85,  0.4,    0.6
  ;; Join_ETS         , 0.2, 0.2,   0.9,    0.4
  ;;
  read-farm-types-and-interventions-from-file word market-data-folder "farmer-threshold-matrix.csv"
  read-interventions-from-file word market-data-folder "intervention-impacts.csv"
  read-thresholds-table-from-file word market-data-folder "farmer-threshold-matrix.csv"
end

;; setup farm types and list of possible interventions
to read-farm-types-and-interventions-from-file [file]
  file-open file
  set farm-types but-first csv:from-row file-read-line
  while [not file-at-end?] [
    create-interventions 1 [
      initialise-intervention item 0 csv:from-row file-read-line
    ]
  ]
  file-close
end

;; initialise the intervention's intervention-effects table
;; which holds the production function impacts by effect type and farm type
to read-interventions-from-file [file]
  ;; CSV file format like this - note that each intervention should be
  ;; 3 consecutive lines with its effects (these can be in any order)
  ;; farm types must be in the order shown (which is the same as the
  ;; order in the farmer thresholds file
  ;;                  ,                    ,   SNB, Dairy, Forest,  Crop
  ;; Build_Wetland    , effect-on-costs    ,    25,    68,     NA,    34
  ;; Build_Wetland    , effect-on-yields   ,  -0.2, -0.02,     NA,  -0.1
  ;; Build_Wetland    , effect-on-emissions,  -0.1, -0.05,     NA, -0.05
  ;; Riparian_Planting, effect-on-costs    ,    26,    71,     NA,    11
  ;; Riparian_Planting, effect-on-yields   ,  -0.2, -0.02,     NA, -0.01
  ;; Riparian_Planting, effect-on-emissions,  -0.1, -0.03,     NA, -0.04
  ;; Clean_Races      , effect-on-costs    ,    NA,    75,     NA,    NA
  ;; Clean_Races      , effect-on-yields   ,    NA,     0,     NA,    NA
  ;; Clean_Races      , effect-on-emissions,    NA, -0.01,     NA,    NA
  ;; Farm_Plan        , effect-on-costs    ,     6,    34,     NA,    NA
  ;; Farm_Plan        , effect-on-yields   , -0.05, -0.01,     NA,    NA
  ;; Farm_Plan        , effect-on-emissions, -0.06, -0.05,     NA,    NA
  ;; Join_ETS         , effect-on-costs    ,     1,    25,     50,   100
  ;; Join_ETS         , effect-on-yields   , -0.25, -0.25,    0.1,  -0.1
  ;; Join_ETS         , effect-on-emissions,  -0.5,  -0.5,    0.1, -0.25
  carefully [
    file-open file
    let the-farm-types but-first but-first csv:from-row file-read-line
    while [not file-at-end?] [
      let i 0
      let the-intervention nobody
      while [i < 3] [
        let data csv:from-row file-read-line
        let i-type item 0 data
        if i = 0 [
          set the-intervention get-intervention i-type
        ]
        set data but-first data
        let the-effect-type item 0 data
        let the-effect-impacts table:from-list zip the-farm-types (but-first data)
        ask the-intervention [
          table:put intervention-effects the-effect-type the-effect-impacts
        ]
        set i i + 1
      ]
    ]
    file-close
  ]
  [
    print "problem reading the interventions effects file"
    file-close
  ]
end

to initialise-intervention [name]
  set intervention-type name
  set intervention-effects table:make
  set hidden? true
  output-print name
end

;; setup baseline probability of adoption of interventions by farm type
to read-thresholds-table-from-file [file]
  set base-thresholds-table table:make
  carefully [
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let rows []
    while [not file-at-end?] [
      let data csv:from-row file-read-line
      let the-intervention item 0 data
      let vals table:from-list zip the-farm-types (but-first data)
      table:put base-thresholds-table the-intervention vals
    ]
    file-close
  ]
  [
    print "problem reading the farmer thresholds data file"
    file-close
  ]
end


;; -----------------------------------------
;; economic setup procedures
;; -----------------------------------------
to setup-economic-parameters
  read-production-function-parameters
  ask patches [ set-farm-production-function ]
end

to read-production-function-parameters
  let yields get-parameter word market-data-folder "commodity-yields.csv"
  set commodity-yield-means item 0 yields
  set commodity-yield-sds item 1 yields

  let costs get-parameter word market-data-folder "input-costs.csv"
  set input-cost-means item 0 costs
  set input-cost-sds item 1 costs

  let emissions get-parameter word market-data-folder "ghg-emissions.csv"
  set ghg-emission-means item 0 emissions
  set ghg-emission-sds item 1 emissions

  set prices              get-prices word market-data-folder "prices.csv"
  set environmental-taxes get-environmental-taxes word market-data-folder "prices.csv"
end

to-report get-parameter [file]
  let means-table table:make
  let sds-table table:make
  carefully [
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let luc 1
    while [not file-at-end?] [
      let means-data but-first csv:from-row file-read-line
      let means table:from-list zip the-farm-types means-data
      table:put means-table luc means
      let sds-data but-first csv:from-row file-read-line
      let sds table:from-list zip the-farm-types sds-data
      table:put sds-table luc sds
      set luc luc + 1
    ]
    file-close
  ]
  [
    print "failed while reading parameter file"
    file-close
  ]
  report (list means-table sds-table)
end

to-report get-prices [file]
  let result nobody
  carefully [
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let price-data but-first csv:from-row file-read-line
    set result table:from-list zip the-farm-types price-data
    file-close
  ]
  [
    print "problem reading prices file"
    file-close
  ]
  report result
end

to-report get-environmental-taxes [file]
  let result nobody
  carefully [
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let x file-read-line ;; skip the prices data
    let tax-data but-first csv:from-row file-read-line
    set result table:from-list zip the-farm-types tax-data
    file-close
  ]
  [
    print "problem reading taxes (prices) file"
    file-close
  ]
  report result
end


;; -----------------------------------------
;; landscape setup
;; NOTE: this includes the farmers not just
;;       LUC codes
;; -----------------------------------------
to setup-geography-from-files
  setup-world-dimensions
  set luc-data gis:load-dataset word spatial-data-folder "luc.asc"
  set parcels-data gis:load-dataset word spatial-data-folder "parcels.shp"

  gis:apply-raster luc-data luc-code
  set farm-land patches with [not is-nan? luc-code and luc-code != 0]
  colour-patches
  display

  gis:apply-coverage parcels-data "ID" temp-ID
  foreach remove-duplicates [temp-ID] of patches [ id ->
    let this-farms-land farm-land with [temp-ID = id]
    if any? this-farms-land [
      let this-farmer nobody
      ask approximate-centroid this-farms-land [
        sprout-farmers 1 [
          initialise-farmer
          set this-farmer self
        ]
      ]
      ask this-farms-land [
        set the-owner this-farmer
      ]
      let this-farm nobody
      ask this-farmer [
        hatch-farms 1 [
          initialise-farm
          set this-farm self
        ]
        set my-farm this-farm
      ]
    ]
  ]
  display
  draw-borders key-border-colour
end

to setup-random-geography
  setup-world-dimensions
  setup-random-luc-codes
  create-farmers 100 [ initialise-farmer ]

  ;; when model is initialised from spatial data including ownership
  ;; and farm boundaries, etc. this code will change
  ;; for now make the farms proximity polygons based on farmer locations
  ask patches [ set the-owner min-one-of farmers [distance myself] ]
  draw-borders key-border-colour ;; this is purely for visualization

  ;; and then we can initialise the farms
  ask farmers [
    let the-farm nobody
    hatch-farms 1 [
      initialise-farm
      set the-farm self
    ]
    set my-farm the-farm
  ]
end

;; use a simple voter model to set up LUC codes for now
;; eventually replace this with reading from GIS files
to setup-random-luc-codes
  ask patches [
    set luc-code one-of n-values 8 [i -> i + 1]
  ]
  set farm-land patches
  repeat luc-aggregation-steps [
    ask patches [
      set luc-code [luc-code] of one-of neighbors4
    ]
  ]
  colour-patches
end

to setup-world-dimensions
  let max-dimension 300
  let cell-size 3
  let ncols 270
  let nrows 400
  let cellsize 100
  if setup-geography-from-files? [
    carefully [
      file-open word spatial-data-folder "luc.asc"
      set ncols read-from-string item 1 split-string file-read-line " "
      set nrows read-from-string item 1 split-string file-read-line " "
      let skip file-read-line
      set skip file-read-line
      set cellsize read-from-string item 1 split-string file-read-line " "
      file-close
    ]
    [ file-close
      print "luc.asc not found"
    ]
  ]
  let x-extent ncols * cellsize
  let y-extent nrows * cellsize
  let x-scale x-extent / max-dimension
  let y-scale y-extent / max-dimension
  let sf ceiling max (list x-scale y-scale) ; 400.333
  resize-world 0 x-extent / sf 0 y-extent / sf ;
  set-patch-size cell-size * max-dimension / max (list world-width world-height)
  ask patches [ set pcolor key-background-colour ]
  display
end

to colour-patches
  ifelse show-luc-codes?
  [ ask farm-land [ set pcolor palette:scale-scheme "Sequential" key-LUC-palette 9 luc-code 1 12 ] ]
  [ ask farm-land [ set pcolor green + 4 ] ]
end


;; -----------------------------------------
;; patch specific functions
;; -----------------------------------------

;; patch procedure
to set-farm-production-function
  ;; this test might need to change depending on how the spatial data files are finally specified
  if not is-nan? luc-code and luc-code != 0 [
    let yield-mean        table:to-list table:get commodity-yield-means luc-code
    let yield-sd          table:to-list table:get commodity-yield-sds luc-code
    set commodity-yields  table:from-list (map [[m s] -> (list item 0 m random-normal item 1 m item 1 s)] yield-mean yield-sd)

    let costs-mean        table:to-list table:get input-cost-means luc-code
    let costs-sd          table:to-list table:get input-cost-sds luc-code
    set input-costs       table:from-list (map [[m s] -> (list item 0 m random-normal item 1 m item 1 s)] costs-mean costs-sd)

    let ghg-mean          table:to-list table:get ghg-emission-means luc-code
    let ghg-sd            table:to-list table:get ghg-emission-sds luc-code
    set ghg-emissions     table:from-list (map [[m s] -> (list item 0 m random-normal item 1 m item 1 s)] ghg-mean ghg-sd)
  ]
end

;; patch-report
to-report get-net-revenue-of-patch [fm-type]
  let the-interventions [my-interventions] of ([my-farm] of the-owner)
  let price             table:get prices fm-type
  ;; yield, costs and emissions are all subject to modification by the interventions
  ;; made on this farm
  let yield             table:get commodity-yields fm-type
  set yield     yield * product [1 + get-intervention-impact fm-type "effect-on-yields"] of the-interventions
  let cost              table:get input-costs fm-type
  set cost       cost + sum [get-intervention-impact fm-type "effect-on-costs"] of the-interventions
  let ghg               table:get ghg-emissions fm-type
  set ghg         ghg * product [1 + get-intervention-impact fm-type "effect-on-emissions"] of the-interventions
  let ghg-tax           table:get environmental-taxes fm-type
  let income price * yield
  let costs cost + ghg * ghg-tax
  report income - costs
end

to-report get-income-from-patch [fm-type]
  let the-interventions [my-interventions] of ([my-farm] of the-owner)
  let price             table:get prices fm-type
  let yield             table:get commodity-yields fm-type
  set yield     yield * product [1 + get-intervention-impact fm-type "effect-on-yields"] of the-interventions
  report price * yield
end

to-report get-costs-of-patch [fm-type]
  let the-interventions [my-interventions] of ([my-farm] of the-owner)
  let cost              table:get input-costs fm-type
  set cost       cost + sum [get-intervention-impact fm-type "effect-on-costs"] of the-interventions
  let ghg               table:get ghg-emissions fm-type
  set ghg         ghg * product [1 + get-intervention-impact fm-type "effect-on-emissions"] of the-interventions
  let ghg-tax           table:get environmental-taxes fm-type
  report cost + ghg * ghg-tax
end

;; -----------------------------------------
;; farm specific functions
;; -----------------------------------------

;; farm 'constuctor'
to initialise-farm
  ;; will be called by the farmer who is 'myself' in this context
  set my-farmer myself
  let the-farmer my-farmer
  set the-land farm-land with [the-owner = the-farmer]
  ifelse not any? the-land
  [ ask my-farmer [ die ]
    die ]
  [ create-link-with my-farmer [set color key-owner-link-colour]
    setxy mean [pxcor] of the-land mean [pycor] of the-land
    set shape "circle"
    let ft one-of farm-types
    set farm-type ft
    ask my-farmer [ set label ft set label-color black ]
    let my-possible-interventions possible-interventions
    set my-interventions turtle-set nobody
;    set my-interventions possible-interventions
;    set my-interventions n-of (random (count my-possible-interventions + 1)) my-possible-interventions
    set available-interventions possible-interventions with [not member? self [my-interventions] of myself]
    set label-color key-label-colour
  ]
end

to implement-intervention [i]
  let the-intervention get-intervention i
  set my-interventions (turtle-set my-interventions the-intervention)
  set available-interventions available-interventions with [self != the-intervention]
end

;; farm reporter
to-report get-net-revenue
  let ft farm-type
  report sum [get-net-revenue-of-patch ft] of the-land
end

to-report get-income
  let ft farm-type
  report sum [get-income-from-patch ft] of the-land
end

to-report get-costs
  let ft farm-type
  report sum [get-costs-of-patch ft] of the-land
end

to update-net-revenue-of-farm
  set net-revenue get-net-revenue
  set size sqrt (abs net-revenue / 2e3) ;; scaling size of net revenue circle
  ifelse net-revenue > 0
  [ set color key-profit-colour ]
  [ set color key-loss-colour ]
  set label (word (count my-interventions) "/" (count my-interventions + count available-interventions))
end

to-report possible-interventions
  report interventions with [is-applicable-to-farm-type? [farm-type] of myself]
end


;; -----------------------------------------
;; farmer specific functions
;; -----------------------------------------

;; farmer 'constructor'
to initialise-farmer
  set size 3
  set color key-farmer-colour
  set shape "person"
  ;; give everyone the default threshold matrix
  ;; perhaps subject to modification later by sigmoid function
  ;; contingent on farmer dispositions (pro-social, pro-environmental, etc.)
  ;  set thresholds-table table:from-json table:to-json base-thresholds-table
  if not setup-geography-from-files? [ place-farmer ]
  ;; setxy random-xcor random-ycor
end

to place-farmer
  while [any? other farmers in-radius farm-centroid-inhibition-distance] [
    setxy random-xcor random-ycor
  ]
end

;; farmer reporter
;; returns threshold for the supplied intervention
to-report get-threshold [i]
  let ft [farm-type] of my-farm
  report table:get (table:get base-thresholds-table i) ft
end

;; farmer command
;; can use this to boost the thresholds associated with this farmer for a given nudge
to-report boosted-threshold [nudge i]
  report nudged-threshold get-threshold i nudge
end

to-report consider-interventions
  let current-costs [get-costs] of my-farm
  let current-income [get-income] of my-farm
  let current-interventions [my-interventions] of my-farm
  let changes []
  let to-consider [available-interventions] of my-farm
  ifelse any? to-consider [
    foreach sort to-consider [ i ->
      let i-type [intervention-type] of i
      let base-probability get-threshold i-type
      ask my-farm [
        set my-interventions (turtle-set current-interventions i)
        let new-costs get-costs
        let new-income get-income
        let change-in-costs precision (100 * (new-costs - current-costs) / current-costs) 1
        let change-in-income precision (100 * (new-income - current-income) / current-income) 1
        let new-prob precision nudged-threshold base-probability (change-in-income - change-in-costs) 3
        set changes lput (list i-type change-in-costs change-in-income new-prob) changes
        set my-interventions current-interventions
      ]
    ]
    report rnd:weighted-one-of-list changes [ [p] -> last p ]
  ]
  [ report nobody ]
end


;; -----------------------------------------
;; intervention specific
;; -----------------------------------------
to-report get-intervention [name]
  report one-of interventions with [intervention-type = name]
end

to-report get-intervention-impact [ft effect]
  report table:get-or-default (table:get intervention-effects effect) ft 0
end

to-report is-applicable-to-farm-type? [ft]
  report not is-string? table:get-or-default (table:get intervention-effects "effect-on-costs") ft "NA"
end


;; -----------------------------------------
;; farmer revenue output for sanity check
;; -----------------------------------------
to-report revenue-summary [is]
  let result (list who farm-type table:get prices farm-type count the-land)
  let current-interventions my-interventions
  set my-interventions turtle-set nobody
  set result lput precision get-costs 1 result
  set result lput precision get-income 1 result
  set result lput precision get-net-revenue 1 result
  foreach but-first is [ i ->
    let the-intervention get-intervention i
    ifelse member? the-intervention possible-interventions [
      set my-interventions turtle-set the-intervention
      set result lput precision get-costs 1 result
      set result lput precision get-income 1 result
      set result lput precision get-net-revenue 1 result
      set my-interventions turtle-set nobody
    ]
    [ set result sentence result ["NA" "NA" "NA"] ]
  ]
  set my-interventions current-interventions
  report result
end

to save-farm-revenue-under-various-interventions [file]
  let sorted-interventions fput "None" sort [intervention-type] of interventions
  let interventions-header sentence ["" "" "" "Interventions"] reduce [ [a b] -> sentence a b ] map [ a -> sentence a ["" ""] ] sorted-interventions
  let cir ["Cost" "Income" "Revenue"]
  let header (sentence ["" "Type" "Price" "Area"] cir cir cir cir cir cir)
  let result (list interventions-header header)
  foreach sort farms [ f ->
    set result lput [revenue-summary sorted-interventions] of f result
  ]
  csv:to-file word output-data-folder file result
end


;; -----------------------------------------
;; utilility functions
;; -----------------------------------------

to-report is-nan? [x]
  report not (x <= 0 or x >= 0)
end

;; zip two lists into a list of tuples (cf. python function)
to-report zip [l1 l2]
  report (map list l1 l2)
end

to-report product [numbers]
  ifelse length numbers = 0
  [ report 1 ]
  [ report reduce * numbers ]
end

to-report sigmoid [x a]
  report 1 / (1 + exp (a * (- x)))
end

to-report logit [x a]
  if x = 0.5 [report 0]
  report ln (x / (1 - x)) / a
end

to-report nudged-threshold [x nudge]
  if x = 1 [report 1]
  if x = 0 [report 0]
  let centre logit x sigmoid-slope
  report sigmoid (centre + nudge) sigmoid-slope
end

to-report approximate-centroid [poly]
  let mean-x mean [pxcor] of poly
  let mean-y mean [pycor] of poly
  report one-of poly with-min [distancexy mean-x mean-y]
end

;; ------------------------------------------
;; drawing utilities (see netlogo-utils repo)
;; ------------------------------------------
to setup-key-colours
  set key-farmer-colour black
  set key-border-colour grey - 2
  set key-owner-link-colour black
  set key-LUC-palette "Greens"
  set key-LUC-palettes table:from-list zip farm-types ["Grays" "Blues" "Oranges" "Greens"]
  set key-profit-colour [ 0 64 160 96 ]
  set key-loss-colour [ 255 0 0 160 ]
  set key-label-colour violet - 2
  set key-background-colour grey
end

to draw-borders [col]
  ifelse setup-geography-from-files?
  [
    gis:set-drawing-color key-border-colour
    gis:draw parcels-data 0.5
  ]
  [
    ;; boundary patches are those with any neighbors4 that have
    ;; different pcolor than themselves
    let boundaries farm-land with [any? neighbors4
      with [[who] of the-owner != [[who] of the-owner] of myself]
    ]
    ask boundaries [
      ask neighbors4 [
        ;; only those with different my-node need to draw a line
        if [who] of the-owner != [[who] of the-owner] of myself [
          draw-line-between self myself col
        ]
      ]
    ]
  ]
end

to-report string-as-list [str]
  report n-values length str [i -> item i str]
end

to-report split-string [str sep]
  let words []
  let this-word ""
  foreach (string-as-list str) [ c ->
    ifelse c = sep
    [ if this-word != ""
      [ set words sentence words this-word
        set this-word ""
      ]
    ]
    [ set this-word word this-word c ]
  ]
  ifelse this-word = ""
  [ report words ]
  [ report sentence words this-word ]
end

;; draw line between two patches
;; by sprouting a turtle and having it move
;; to halfway point and draw the edge
to draw-line-between [p1 p2 col]
  ;; set a visible colour
  let pen-color col
  ask p1 [
    ;; make a turtle to do the drawing
    sprout 1 [
      set color pen-color
      ;; move to the boundary
      face p2
      jump 0.5
      ;; face the corner and move there
      rt 90
      jump 0.495
      ;; turn around and draw the edge
      rt 180
      pd
      jump .99
      ;; and die...
      die
    ]
  ]
end


to profile
  profiler:start         ;; start profiling
  setup                  ;; set up the model
  profiler:stop          ;; stop profiling
  print profiler:report  ;; view the results
  profiler:reset         ;; clear the data
end

;; The MIT License (MIT)
;;
;; Copyright (c) 2023 David O'Sullivan
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to  permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
@#$#@#$#@
GRAPHICS-WINDOW
184
10
654
908
-1
-1
3.0
1
8
1
1
1
0
0
0
1
0
155
0
299
1
1
1
ticks
30.0

BUTTON
54
35
120
68
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
809
15
981
48
sigmoid-slope
sigmoid-slope
0.01
2
1.0
0.01
1
NIL
HORIZONTAL

SWITCH
807
523
954
556
seed-the-rng?
seed-the-rng?
1
1
-1000

INPUTBOX
960
523
1034
583
rng-seed
145.0
1
0
Number

TEXTBOX
961
589
1044
636
Integer value value for RNG seed
11
0.0
1

OUTPUT
805
118
1013
240
11

TEXTBOX
812
85
962
113
Interventions\n(for information)
11
0.0
1

SWITCH
801
283
1041
316
setup-geography-from-files?
setup-geography-from-files?
0
1
-1000

SLIDER
805
422
1032
455
luc-aggregation-steps
luc-aggregation-steps
0
100
50.0
1
1
NIL
HORIZONTAL

TEXTBOX
801
399
951
417
Random landscape
12
0.0
1

SLIDER
804
461
1033
494
farm-centroid-inhibition-distance
farm-centroid-inhibition-distance
0
sqrt (count patches / 100 / pi)
2.8
0.1
1
NIL
HORIZONTAL

SWITCH
11
540
175
573
show-luc-codes?
show-luc-codes?
0
1
-1000

TEXTBOX
804
506
844
524
RNGs
12
0.0
1

BUTTON
93
582
167
615
redraw
colour-patches\ndisplay
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
56
92
119
125
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
58
136
121
169
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
807
326
945
371
region
region
"Rangitaiki"
0

BUTTON
20
488
169
521
toggle-labels
set show-labels? not show-labels?\nask turtles [\n  set label-color ifelse-value show-labels? [key-label-colour] [[0 0 0 0]]\n]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
20
445
168
478
toggle-farmers
ask farmers [\n  set hidden? not hidden?\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

hex
false
0
Polygon -7500403 true true 0 150 75 20 225 20 300 150 225 280 75 280

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
