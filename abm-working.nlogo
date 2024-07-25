extensions [
  matrix   ;; matrix maths
  csv      ;; easy reading of CSVs
  palette  ;; nicer colours
  table    ;; dictionary like tables for storing lots of the input parameters
  rnd      ;; weighted random draws
  gis      ;; spatial data
  profiler ;; in case we need to figure out why things are slow
]

globals [
  output-data-folder
  market-data-folder
  spatial-data-folder
  show-labels?
  interventions-lookup    ;; interventions in a table so they can be retrieved by name

  epsilon                 ;; for convenience - constant for 'smallest number'

  parcels-data            ;; GIS functionality depends on reading the data into an object
  luc-data                ;; then acccesing it again later

  farm-land               ;; patches that are on farms
  not-farm-land

  farm-types              ;; list of named farm types
  commodity-yield-means   ;; table of mean yields by LUC and farm-type
  commodity-yield-sds     ;; table of sd of yields by LUC and farm-type
  input-cost-means        ;; table of mean input costs by LUC and farm-type
  input-cost-sds          ;; table of sd of input costs by LUC and farm-type
  ghg-emission-means      ;; table of mean GHG emissions by LUC and farm-type
  ghg-emission-sds        ;; table of sd of GHG emissions by LUC and farm-type
  prices                  ;; table of commodity prices
  base-thresholds         ;; table of default farmer decision thresholds for various interventions

  ;; matrix equivalents of the above -- which after initialisation have priority!
  m-yield-means
  m-yield-sds
  m-cost-means
  m-cost-sds
  m-emission-means
  m-emission-sds
  m-prices
  m-intervention-cost-impacts
  m-intervention-yield-impacts
  m-intervention-emissions-impacts

  ;; colour settings - these can be changed in one place, see setup-key-colours procedure
  colour-key              ;; table of colour settings
]

breed [farmers farmer]
breed [farms farm]        ;; representative turtle for the farm - for data storage...
breed [interventions
       intervention]      ;; place to keep relevant data about each intervention type

farms-own [
  my-farmer               ;; the farmer who owns/runs this farm
  farm-type               ;; farm type of this farm
  the-land                ;; patch-set of the patches in this farm
  landuse-luc-profile
  current-profit          ;; profit of farm summed across patches
  current-income          ;; income of farm summed across patches
  current-costs           ;; costs of farm summed across patches
  my-interventions
  available-interventions
  ;; initial values of the above
  farm-type-0
  current-profit-0
  current-income-0
  current-costs-0
  my-interventions-0
  available-interventions-0
]

farmers-own [
  my-farm                 ;; the farm turtle of this farmer's farm
]

interventions-own [
  intervention-type       ;; this would be one of the named types
  cost-impacts
  yield-impacts
  emissions-impacts
;  intervention-impacts    ;; table of effects by farm-type and outcome (costs, yields, emissions)
]

patches-own [
  temp-ID
  the-owner               ;; the farmer who owns this patch
  luc-code                ;; LUC code where 1 = LUC1, 2 = LUC2, etc.
  landuse
  landuse-0
]

;; -----------------------------------------
;; MAIN LOOP i.e., setup and go
;; -----------------------------------------
to setup
  ;; Setup order is very sensitive to a number of interdependencies among the various
  ;; elements. So be VERY CAREFUL IF CHANGING THE SEQUENCE OF OPERATIONS in this procedure
  clear-all

  set epsilon 1e-16
  set show-labels? false
  set interventions-lookup table:make

  let base-data-folder "data/"
  set output-data-folder word base-data-folder "output/"                ;; 'data/output/'
  set market-data-folder word base-data-folder "market/"                ;; 'data/market/'
  set spatial-data-folder (word base-data-folder "spatial/" region "/") ;; 'data/spatial/REGION-NAME/'

  setup-world-dimensions

  setup-farmer-parameters
  setup-colours
  ask patches [ set pcolor table:get colour-key "background" ]
  setup-geography
  assign-patches-to-farmers
  assign-farms-to-farmers
  setup-economic-parameters
  make-matrix-copies-of-data
  redraw
  reset-ticks
  random-seed run-rng-seed
  go ;; this initialises the farms with current net profit and some interventions
  store-initial-values
  if run-rng-seed != 0 [ random-seed run-rng-seed ]
end

;; the main model loop
to go
  ifelse stop-model? [
    cleanup
    stop
  ]
  [
    ;; much more action to go here...
    ask farms [
      update-profit-of-farm
    ]
    ask farmers [
      if [length get-available-interventions] of my-farm > 0 [
        let potential-change consider-interventions false
        if random-float 1 < last potential-change [
          if show-interventions? [
            print (
              word "Farmer " who " implementing "
              first potential-change " on "
              [farm-type] of my-farm " " [who] of my-farm)
          ]
          ask my-farm [
            implement-intervention first potential-change
          ]
        ]
      ]
    ]
    tick
  ]
end

;; put a model stop condition here
to-report stop-model?
  ifelse all-true? [length get-available-interventions = 0] of farms [
    show "Stopping model: all possible interventions implemented on all farms!"
    report true
  ]
  [
    report false
  ]
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
  ;; interventions as row names, i.e, (spaces added for clarity)
  ;;
  ;;                  , SNB, Dairy, Forest, Crop
  ;; Build_Wetland    , 0.7, 0.75,  0.3,    0.5
  ;; Riparian_Planting, 0.7, 0.75,  0.2,    0.4
  ;; Clean_Races      , 0.2, 0.7,   0,      0
  ;; Farm_Plan        , 0.7, 0.85,  0.4,    0.6
  ;; Join_ETS         , 0.2, 0.2,   0.9,    0.4
  ;;
  read-farm-types-from-file word market-data-folder "farmer-threshold-matrix.csv"
  read-interventions-from-file word market-data-folder "farmer-threshold-matrix.csv"
  read-farmer-thresholds-from-file word market-data-folder "farmer-threshold-matrix.csv"
  read-intervention-impacts-from-file word market-data-folder "intervention-impacts.csv"
end

;; setup farm types and interventions
to read-farm-types-from-file [file]
  carefully [
    print word "Reading farm types from " file
    file-open file
    set farm-types but-first csv:from-row file-read-line
    file-close
  ]
  [
    print "ERROR: problem reading farm types from thresholds data file"
    file-close
  ]
end

;; setup interventions
to read-interventions-from-file [file]
  carefully [
    print word "Reading intervention types from " file
    file-open file
    let skip file-read-line
    while [not file-at-end?] [
      create-interventions 1 [
        initialise-intervention item 0 csv:from-row file-read-line
      ]
    ]
    file-close
  ]
  [
    print "ERROR: problem reading intervention types from thresholds data file"
    file-close
  ]
end

;; setup baseline probability of adoption of interventions by farm type
to read-farmer-thresholds-from-file [file]
  ;; CSV file looks like this (spacing added for clarity)
  ;;
  ;;                   , SNB, Dairy, Forest, Crop
  ;;  Build_Wetland    , 0.7,  0.75,    0.3,  0.5
  ;;  Riparian_Planting, 0.7,  0.75,    0.2,  0.4
  ;;  Clean_Races      , 0.2,  0.7 ,    0  ,  0
  ;;  Farm_Plan        , 0.7,  0.85,    0.4,  0.6
  ;;  Join_ETS         , 0.2,  0.2 ,    0.9,  0.4
  ;;
  ;; resulting table is read by
  ;;   table-get (table:get base-thresholds INTERVENTION-NAME) FARM-TYPE
  set base-thresholds table:make
  carefully [
    print word "Reading farmer adoption thresholds from file " file
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    while [not file-at-end?] [
      let data csv:from-row file-read-line
      let the-intervention item 0 data
      let vals table:from-list zip the-farm-types (but-first data)
      table:put base-thresholds the-intervention vals
    ]
    file-close
  ]
  [
    print "ERROR: problem reading the farmer thresholds data file"
    file-close
  ]
end

;; initialise the global intervention-effects table
;; which holds production function impacts by intervention type, effect, and farm type
to read-intervention-impacts-from-file [file]
  ;; CSV file format like this - note that each intervention should be
  ;; 3 consecutive lines with its effects (these can be in any order)
  ;; farm types can also be in any order. Spaces added for clarity
  ;;
  ;; Intervention     , Impact   ,   SNB, Dairy, Forest,  Crop
  ;; Build_Wetland    , costs    ,    25,    68,     NA,    34
  ;; Build_Wetland    , yields   ,  -0.2, -0.02,     NA,  -0.1
  ;; Build_Wetland    , emissions,  -0.1, -0.05,     NA, -0.05
  ;; Riparian_Planting, costs    ,    26,    71,     NA,    11
  ;; Riparian_Planting, yields   ,  -0.2, -0.02,     NA, -0.01
  ;; Riparian_Planting, emissions,  -0.1, -0.03,     NA, -0.04
  ;;  ... and so on ...
  ;;
  carefully [
    print word "Reading intervention impacts from " file
    file-open file
    let the-farm-types but-first but-first csv:from-row file-read-line
    while [not file-at-end?] [
      let i 0
      let i-type nobody
      while [i < 3] [ ;; read the table in 3s
        let data csv:from-row file-read-line
        set i-type item 0 data
        set data but-first data
        let the-effect-type item 0 data
        let the-effect-impacts table:from-list zip the-farm-types (but-first data)
        set-intervention-impact i-type the-effect-type the-effect-impacts
        set i i + 1
      ]
    ]
    file-close
  ]
  [
    print "ERROR: problem reading the interventions effects file"
    file-close
  ]
end


;; -----------------------------------------
;; economic setup procedures
;; -----------------------------------------
to setup-economic-parameters
  with-local-randomness [
    ifelse seed-setup-rng?
    [ random-seed rng-economics ]
    [ random-seed timer ]
    read-production-function-parameters
    ask farms [ set landuse-luc-profile get-farm-landuse-luc-profile ]
  ]
end

to read-production-function-parameters
  let yields get-parameter-from-file word market-data-folder "commodity-yields.csv"
  set commodity-yield-means item 0 yields
  set commodity-yield-sds   item 1 yields

  let costs get-parameter-from-file word market-data-folder "input-costs.csv"
  set input-cost-means      item 0 costs
  set input-cost-sds        item 1 costs

  let emissions get-parameter-from-file word market-data-folder "ghg-emissions.csv"
  set ghg-emission-means    item 0 emissions
  set ghg-emission-sds      item 1 emissions

;  let prices-data           get-prices word market-data-folder "prices.csv"
;  set prices                item 0 prices-data
;  set environmental-taxes   item 1 prices-data
  set prices           get-prices word market-data-folder "prices.csv"
end

;; return mean and sd tables for a parameter from a file
to-report get-parameter-from-file [file]
  ;; each parameter CSV file has the same format
  ;; shown here with spaces added for clarity, and
  ;; extending to 8 LUC levels. Cols can be in any order
  ;; BUT LUC mean/sd rows MUST be as shown.
  ;;
  ;;          , SNB , Dairy, Forest, Crop
  ;; LUC1_Mean, 3500,  9500,   4000, 3000
  ;; LUC1_SD  ,  700,  1900,    800,  600
  ;; LUC2_Mean, 2650,  8050,   4000, 2850
  ;; LUC2_SD  ,  530,  1610,    800,  570
  ;; LUC3_Mean, 1625,  7350,   3850, 2675
  ;; LUC3_SD  ,  325,  1470,    770,  535
  ;;   ... and so on to LUC8 ...
  ;;
  ;; Returned tables are readable as
  ;;   table:get (table:get TABLE LUC-CODE) FARM-TYPE
  let means-table table:make
  let sds-table table:make
  carefully [
    print word "Reading parameters from " file
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let luc 1
    while [not file-at-end?] [
      ;; read the LUCx_Mean line
      let means-data but-first csv:from-row file-read-line
      let means table:from-list zip the-farm-types means-data
      table:put means-table luc means
      ;; read the LUCx_SD line
      let sds-data but-first csv:from-row file-read-line
      let sds table:from-list zip the-farm-types sds-data
      table:put sds-table luc sds
      set luc luc + 1
    ]
    file-close
  ]
  [
    print word "ERROR: failed while reading parameter file " file
    file-close
  ]
  report (list means-table sds-table)
end

to-report get-prices [file]
  ;; the CSV for prices and emissions taxes looks like this
  ;; (spacing addded for clarity). No checking that the rows
  ;; are in this order
  ;;
  ;;               ,SNB,Dairy, Forest, Crop
  ;;Price_Commodity,  5,  7.5,   157,   0.5
  ;;Price_GhG      , 25, 25  ,    25,  25
  ;;Price_Nleach   ,  0,  0  ,     0,   0
  ;; ... other redundant lines ...
  ;;
  ;; NOTE: the taxes part of this has been replaced by a slider
  ;; since carbon taxes are a fixed price across all landuses
  ;;
  let prices-table nobody
  let taxes-table nobody
  carefully [
    print word "Reading prices from " file
    file-open file
    let the-farm-types but-first csv:from-row file-read-line
    let price-data but-first csv:from-row file-read-line
    set prices-table table:from-list zip the-farm-types price-data
;    let taxes-data but-first csv:from-row file-read-line
;    set taxes-table table:from-list zip the-farm-types taxes-data
    file-close
  ]
  [
    print "ERROR: problem reading prices file"
    file-close
  ]
;  report list prices-table taxes-table
  report prices-table
end


;; -----------------------------------------
;; landscape setup
;; NOTE: this includes the farmers not just
;;       LUC codes
;; -----------------------------------------
to setup-geography
  with-local-randomness [
    ifelse setup-geography-from-files?
    [ setup-geography-from-files ]
    [ setup-random-geography ]
    ;; assign patches to farmers to make farms
    colour-patches true
  ]
end

to setup-geography-from-files
  carefully [
    print (word "Reading LUC data from " spatial-data-folder "luc.asc")
    set luc-data gis:load-dataset word spatial-data-folder "luc.asc"
    print (word "Reading farm data from " spatial-data-folder "parcels.shp")
    set parcels-data gis:load-dataset word spatial-data-folder "parcels.shp"
  ]
  [
    print "ERROR: problem reading spatial data"
  ]
  gis:apply-raster luc-data luc-code
  set farm-land patches with [not is-nan? luc-code and luc-code > 0]
  colour-patches false
  display ;; do this early for reassurance...
  let bgcolor table:get colour-key "background"
  set not-farm-land patches with [pcolor = bgcolor]

  ;; setup farms based on distinct parcels identified by ID in the parcels data
  ;; This relies on a STR_ID variable which is unique string for each farm parcel
  ;; String is IMPORTANT because of automatic interpolation that GIS extension applies
  ;; when assigning values from a shapefile to patches only partly contained in polygons
  gis:apply-coverage parcels-data "STR_ID" temp-ID
  ;; might get some new NaN values, so clean up farm-land and not-farm-land patch-sets
  let more-not-farm-land farm-land with [not is-string? temp-ID]
  ask more-not-farm-land [ set pcolor bgcolor ]
  set not-farm-land (patch-set not-farm-land more-not-farm-land)
  ;; could use not-farm-land membership in border drawing but it is slow, so...
  ask not-farm-land [ set the-owner nobody ]
  set farm-land farm-land with [not member? self more-not-farm-land]
  ask farm-land [ set temp-ID read-from-string temp-ID ]
end

to setup-random-geography
  ifelse seed-geography-rng?
  [ random-seed rng-geography ]
  [ random-seed random timer ]
  set farm-land patch-set patches
  set not-farm-land patch-set nobody
  setup-random-luc-codes
  create-farmers 250 [ initialise-farmer ]
  colour-patches false
  display
  ;; make the farms proximity polygons based on farmer locations
  ask patches [ set temp-ID [who] of min-one-of (farmers with-min [distance myself]) [who] ]
;  ask patches [ set the-owner min-one-of farmers [distance myself] ]
end

;; use a simple voter model to set up LUC codes for now eventually
;; replace this with reading from GIS files or perhaps use a different
;; method (hence it is separate function)
to setup-random-luc-codes
  set farm-land patches
  ifelse random-landscape-method = "averaging" [
    ask patches [ set luc-code random-normal 0 1 ]
    repeat luc-aggregation-steps [ diffuse4 luc-code 0.8 ]
    let min-x min [luc-code] of patches
    let max-x max [luc-code] of patches
    ask patches [
      set luc-code round rescale luc-code min-x max-x 0.5 8.4999
    ]
  ]
  [ ;; voter model method
    ask patches [ set luc-code 1 + random 8 ]
    repeat luc-aggregation-steps [
      ask patches [ set luc-code [luc-code] of one-of neighbors4 ]
    ]
  ]
  colour-patches false
  display
end

to assign-patches-to-farmers
  ;; do this with a list to ensure assignment is repeatable
  clear-drawing
  foreach remove-duplicates [temp-ID] of farm-land [ id ->
    let this-farms-land farm-land with [temp-ID = id]
    if any? this-farms-land [
      let this-farmer nobody
      ifelse setup-geography-from-files? [
        ask approximate-centroid this-farms-land [
          sprout-farmers 1 [
            initialise-farmer
            set this-farmer self
          ]
        ]
      ]
      [ set this-farmer farmer id
        ask this-farmer [ move-to approximate-centroid this-farms-land ]
      ]
      ask this-farms-land [
        set the-owner this-farmer
      ]
    ]
  ]
  draw-borders table:get colour-key "border"
end

;; returns approximate centroid of a patch-set
to-report approximate-centroid [poly]
  let centroid nobody
  with-local-randomness [
    let mean-x mean [pxcor] of poly
    let mean-y mean [pycor] of poly
    set centroid one-of poly with-min [distancexy mean-x mean-y]
  ]
  report centroid
end

to assign-farms-to-farmers
  ;; do this with a list to ensure assignment is repeatable
  ask farmers [
    let this-farm nobody
    hatch-farms 1 [
      initialise-farm
      set this-farm self
    ]
    set my-farm this-farm
    set hidden? true
  ]
end

;; this works OK, but might not be sensible to do...
;; note that set-world-size kills all turtles so this must
;; be done BEFORE any turtles are initialised
to setup-world-dimensions
  let cell-size 3
  let ncols 270
  let nrows 400
  let raster-pixel-resolution 3
  ;; if initialising from geospatial data we override some of these defaults
  if setup-geography-from-files? [
    print "Getting model dimensions from GIS data"
    ;; read dimensions of the raster from header of an Esri raster .asc file
    ;; which looks like this
    ;;
    ;; ncols         623
    ;; nrows         1201
    ;; xllcorner     1883097.0153
    ;; yllcorner     5676986.4123
    ;; cellsize      100
    ;; NODATA_value  -9999
    ;;
    carefully [
      file-open word spatial-data-folder "luc.asc"
      set ncols read-from-string item 1 split-string file-read-line " "
      set nrows read-from-string item 1 split-string file-read-line " "
      let skip file-read-line
      set skip file-read-line
      set raster-pixel-resolution read-from-string item 1 split-string file-read-line " "
      file-close
    ]
    [
      file-close
      print (word "Problem reading " spatial-data-folder "luc.asc")
    ]
  ]
  let x-extent ncols * raster-pixel-resolution
  let y-extent nrows * raster-pixel-resolution
  let x-scale x-extent / max-dimension
  let y-scale y-extent / max-dimension
  let sf max (list x-scale y-scale)
  let x (list 0 (round (x-extent / sf) - 1))
  let y (list 0 (round (y-extent / sf) - 1))
  show csv:to-row (list ncols nrows raster-pixel-resolution x-extent y-extent x-scale y-scale sf x y)
  resize-world item 0 x item 1 x item 0 y item 1 y
  print (word "Setting model dimensions to " x " by " y )
  set-patch-size cell-size * max-dimension / max (list world-width world-height)
  ask patches [ set pcolor grey ]
end


;; -----------------------------------------
;; patch specific functions
;; -----------------------------------------

;; patch reporter
to-report get-farm-of-patch
  report [my-farm] of the-owner
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
  [ ;; a null farm (which can happen due to random placement of farmers
    ;; or parcels in GIS data below resolution of the patch grid
    ask my-farmer [ die ]
    die ]
  [
    setxy mean [pxcor] of the-land mean [pycor] of the-land
    set shape "square 3"
    set hidden? false
    set farm-type one-of farm-types
    let ft farm-type
    ask the-land [ set landuse ft ]
    set label farm-type
    set label-color ifelse-value show-labels? [table:get colour-key "label"] [[0 0 0 0]]
    set my-interventions matrix:make-constant 1 length table:keys interventions-lookup 0
    set available-interventions possible-interventions
  ]
end

to implement-intervention [i-type]
  matrix:set my-interventions 0 position i-type table:keys interventions-lookup 1
  matrix:set available-interventions 0 position i-type table:keys interventions-lookup 0
end

;; this alway sapplies year-on-year SD so no 'with-var?' parameter required
to update-profit-of-farm
  set current-income get-farm-income true
  set current-costs get-farm-costs true
  set current-profit current-income - current-costs
  redraw-farm
end

to-report get-my-interventions
  report slice-by-ones
    table:keys interventions-lookup
    item 0 matrix:to-row-list my-interventions
end

to-report get-available-interventions
  report slice-by-ones
    table:keys interventions-lookup
    item 0 matrix:to-row-list available-interventions
end

to-report possible-interventions
  let ft farm-type
  report matrix:from-row-list
    (list map [i -> ifelse-value [is-applicable-to-farm-type? ft] of i [1] [0]] sort interventions)
end

to-report get-intervention-score [i-type show-messages?]
  matrix:set my-interventions 0 position i-type table:keys interventions-lookup 1
  let new-costs get-farm-costs false
  let new-income get-farm-income false
  let change-in-costs relative-change current-costs new-costs
  let change-in-income relative-change current-income new-income
  if show-messages? [
    show i-type
    show (word "Current costs: " current-costs   " Current income: " current-income)
    show (word "New costs    : " new-costs       " New income    : " new-income)
    show (word "Delta costs  : " change-in-costs " Delta income  : " change-in-income)
  ]
  ;; don't forget to undo the intervention... we are only trying them out!
  matrix:set my-interventions 0 position i-type table:keys interventions-lookup 0
  report change-in-income - change-in-costs
end

to-report get-farm-landuse-profile
  report matrix:from-column-list (list matrix-column-sums landuse-luc-profile)
end

to-report get-farm-luc-profile
  report matrix:from-column-list (list matrix-row-sums landuse-luc-profile)
end

to-report get-farm-landuse-luc-profile
  let luc-i map [p -> position [luc-code] of p range-from-to 1 9] sort the-land
  let lu-j map [p -> position [landuse] of p farm-types] sort the-land
  let nrow 8
  let ncol length farm-types
  let farm-profile matrix:make-constant nrow ncol 0
  (foreach luc-i lu-j [ [i j] ->
    matrix:set farm-profile i j matrix:get farm-profile i j + 1
  ])
  report farm-profile
end


;; -----------------------------------------
;; farmer specific functions
;; -----------------------------------------

;; farmer 'constructor'
to initialise-farmer
  set size 2
  set color table:get colour-key "farmer"
  set shape "person"
  set hidden? true
  ;; give everyone the default threshold matrix
  ;; perhaps subject to modification later by sigmoid function
  ;; contingent on farmer dispositions (pro-social, pro-environmental, etc.)
  ;; if random setup then put them somewhere random
  if not setup-geography-from-files? [ place-farmer ]
end

;; pick a random location for farmer (used in random initialisation)
to place-farmer
  while [any? other farmers in-radius inhibition-distance] [
    setxy random-xcor random-ycor
  ]
end

;; farmer reporter
;; returns threshold for the supplied intervention
;; this or a wrapper for this could include farmer demography 'shifts'
;; or social/geographic network shifts
to-report get-adoption-probability [i-type]
  report table:get (
    table:get base-thresholds i-type
  ) [farm-type] of my-farm
end

;; loop through the available interventions scoring their probabilities
;; then select one in a random draw weighted by those probabilities
to-report consider-interventions [show-messages?]
  let fm my-farm
  ;; list the interevention types that are to be considered
;  let to-consider [sort available-interventions] of fm
  let to-consider [get-available-interventions] of fm
  ifelse length to-consider > 0 [
    ;; get nudge score for this intervention from the farm
    let scores map [poss -> [get-intervention-score poss show-messages?] of fm] to-consider
    ;; get the base probabilities for this farmer (which might be changeable over time)
    let base-probs map [poss -> get-adoption-probability poss] to-consider
    ;; adjust probabilities using the sigmoid function
    let new-probs (map [[prob score] -> nudged-probability prob score] base-probs scores)
    if show-messages? [
      show [farm-type] of fm
      show to-consider
      show map [s -> precision s 4] scores
      show map [p -> precision p 4] base-probs
      show map [p -> precision p 4] new-probs
    ]
    ;; put results in a list of tuples [[<INTERVENTION> <PROBABILITY>], select one
    ;; by weighted random draw and return it to the calling context
    let changes zip to-consider new-probs
    report rnd:weighted-one-of-list changes [ [p] -> last p ]
  ]
  [ report nobody ]
end


;; -----------------------------------------
;; intervention specific
;; -----------------------------------------

;; initialises an intervention with a name and blank table for effects
to initialise-intervention [name]
  set intervention-type name
  table:put interventions-lookup name self
;  set intervention-impacts table:make
  set hidden? true
  output-print name
end

;; it's convenient sometimes to be able to get an intervention by its name
to-report get-intervention [name]
  report table:get interventions-lookup name
end

;; set impacts (a farm-type: value lookup)
to set-intervention-impact [i-type effect-type impacts]
  ask get-intervention i-type [
;    table:put intervention-impacts effect-type impacts
    (ifelse
      effect-type = "costs"     [ set cost-impacts impacts ]
      effect-type = "yields"    [ set yield-impacts impacts ]
      effect-type = "emissions" [ set emissions-impacts impacts ]
      [ user-message "Unexpected effect-type specified in set-intervention-impact procedure" ]
    )
  ]
end

;; get the impact of this intervention by farm type and effect (cost/yield/emissions)
;; return 0 if no effect -- this won't affect any calculations
to-report get-intervention-impact [ft effect]
;  report table:get-or-default (
;    table:get intervention-impacts effect
;  ) ft 0
  (ifelse
    effect = "costs"     [ report table:get-or-default cost-impacts ft 0 ]
    effect = "yields"    [ report table:get-or-default yield-impacts ft 0 ]
    effect = "emissions" [ report table:get-or-default emissions-impacts ft 0 ]
    [ user-message "Unexpected effect-type specified in get-intervention-impact reporter"
      report nobody ]
  )
end

;; tests if intervention is applicable by checking if the result is in the impacts table
to-report is-applicable-to-farm-type? [ft]
;  report is-number? table:get-or-default (
;    table:get intervention-impacts "costs" ;; pick one at random
;  ) ft "NA"
  report is-number? table:get-or-default cost-impacts ft "NA"
end


;; -----------------------------------------
;; matrix alternatives
;; -----------------------------------------

;; There is potential in matrix approaches for speeding up the calculations and
;; perhaps even making them more 'sophisticated'. However some of this may come
;; at the cost of (i) less readable code, and (ii) less variance. On (ii) specifically
;; there is now no base patch level variance, i.e. the mean yield, emissions and
;; input costs of every patch of given landuse and LUC combination is effectively
;; identical - not initialised with the supplied SD. The SD is applied to patches
;; each round BUT AT FARM LEVEL. That is, every patch on a given farm with given
;; LU/LUC experiences same deviation from mean outcomes in a particular year. But a
;; neighbouring farm might experience quite different deviations.
to make-matrix-copies-of-data
  set m-yield-means matrix:from-row-list map [t -> table:values t] table:values commodity-yield-means
  set m-yield-sds matrix:from-row-list map [t -> table:values t] table:values commodity-yield-sds
  set m-cost-means matrix:from-row-list map [t -> table:values t] table:values input-cost-means
  set m-cost-sds matrix:from-row-list map [t -> table:values t] table:values input-cost-sds
  set m-emission-means matrix:from-row-list map [t -> table:values t] table:values ghg-emission-means
  set m-emission-sds matrix:from-row-list map [t -> table:values t] table:values ghg-emission-sds
  ;; TODO: change m-prices to a single row vector one entry per land use, not this 8x duplicated thing
  set m-prices matrix:from-column-list (list table:values prices)
;  set m-taxes matrix-from-list rep-list-inline table:values environmental-taxes 8 8 4 true
  set m-intervention-cost-impacts
    matrix:from-row-list map [i -> table:values [cost-impacts] of i] sort interventions
  ;; note that these two are multiplicatively applied so add 1 to 0.05 -> 1.05, 0 -> 1, etc
  set m-intervention-yield-impacts
    matrix:plus 1 matrix:from-row-list map [i -> table:values [yield-impacts] of i] sort interventions
  set m-intervention-emissions-impacts
    matrix:plus 1 matrix:from-row-list map [i -> table:values [emissions-impacts] of i] sort interventions
end

;to-report get-farm-profit [with-var?]
;  report get-farm-income with-var? - get-farm-costs with-var?
;end

;; for now this has no interventions component
to-report get-farm-income [with-var?]
  ifelse with-var?
  [
    report round matrix-sum-elements (
      ( matrix:map [[a b c] -> a * b * c]
        landuse-luc-profile
        matrix:plus m-yield-means get-farm-yield-variance
        get-yield-after-interventions ) matrix:* m-prices )
  ]
  [
    report round matrix-sum-elements (
      ( matrix:map [[a b c] -> a * b * c]
        landuse-luc-profile
        m-yield-means
        get-yield-after-interventions ) matrix:* m-prices )
  ]
end

;; for now this has no interventions component
to-report get-farm-costs [with-var?]
  ifelse with-var?
  [
    report round ( get-cost-after-interventions + matrix-sum-elements
      ( matrix:map *
        landuse-luc-profile
        ( m-cost-means matrix:+
          get-farm-cost-variance matrix:+
          ( matrix:map *
              ( matrix:plus
                m-emission-means
                get-farm-emissions-variance )
              get-emissions-after-interventions ) matrix:* carbon-price
        )
      )
    )
  ]
  [
    report round ( get-cost-after-interventions + matrix-sum-elements
      ( matrix:map *
        landuse-luc-profile
        ( m-cost-means matrix:+
          ( matrix:map *
              m-emission-means
              get-emissions-after-interventions ) matrix:* carbon-price
        )
      )
    )
  ]
end

;; Reduced variance normal deviates for sample size n
;; This simulates the central tendency effect of deviations from mean
;; across farms of varying size. So a small farm with only a single patch
;; of given LU/LUC might see a very large negative or positive deviation
;; from mean outcomes. But a large farm with n patches of LU/LUC has the
;; deviation downscaled by √n -- this is of course summed across n patches
;; so the overall deviation will be larger, but appropriately 'smoothed'
;; for the larger holding.
to-report reduced-normal-deviate [m s n]
  if n <= 1 [ report random-normal m s ]
  report random-normal m (s / sqrt(n))
end

to-report get-farm-yield-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-yield-sds landuse-luc-profile)
end

to-report get-farm-cost-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-cost-sds landuse-luc-profile)
end

to-report get-farm-emissions-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-emission-sds landuse-luc-profile)
end

to-report get-cost-after-interventions
  report round matrix-sum-elements
         ( my-interventions matrix:*
           m-intervention-cost-impacts matrix:*
           get-farm-landuse-profile )
end

;; reports a matrix for use in multiplication of yields
;; contingent on on-farm interventions
to-report get-yield-after-interventions
  report matrix:transpose matrix-dup-cols ( matrix-column-products (
    ( matrix:map [[a b] -> ifelse-value a = 0 [1] [b]]
      matrix-dup-cols matrix:transpose my-interventions length farm-types
      m-intervention-yield-impacts )
    ) ) 8
end

;; ditto for emissions
to-report get-emissions-after-interventions
  report matrix:transpose matrix-dup-cols ( matrix-column-products (
    ( matrix:map [[a b] -> ifelse-value a = 0 [1] [b]]
      matrix-dup-cols matrix:transpose my-interventions length farm-types
      m-intervention-emissions-impacts )
    ) ) 8
end


;; -----------------------------------------
;; model rendering
;; -----------------------------------------

to redraw
  colour-patches show-landuse?
  redraw-farms
  display
end

;; colour in the map by landuse and LUC
to colour-patches [colour-by-type?]
  with-local-randomness [
    ifelse colour-by-type? and show-landuse? [
      ifelse show-luc-codes? [
        foreach table:to-list table:get colour-key "farm-type-palettes" [ farm-type-palette ->
          let ft item 0 farm-type-palette
          let pal-name item 1 farm-type-palette
          ask farm-land with [landuse = ft] [
            set pcolor palette:scale-scheme "Sequential" pal-name 9 luc-code 9 0
          ]
        ]
      ]
      [
        foreach table:to-list table:get colour-key "farm-type" [ farm-type-colours ->
          let ft item 0 farm-type-colours
          let col item 1 farm-type-colours
          ask farm-land with [landuse = ft] [
            set pcolor col
          ]
        ]
      ]
    ]
    [
      let pal table:get colour-key "LUC-palette"
      ifelse show-luc-codes?
      [ ask farm-land [ set pcolor palette:scale-scheme "Sequential" pal 9 luc-code 9 0 ] ]
      [ ask farm-land [ set pcolor green + 2 ] ]
    ]
  ]
end

to redraw-farm
  set size (abs current-profit / 250) ^ 0.333 ;; scaling size of net profit circle
  ifelse current-profit > 0 [
    ifelse farm-type-colours?
    [ set color table:get table:get colour-key "farm-type" farm-type
      set color color palette:with-alpha 160
    ]
    [ set color table:get colour-key "profit" ]
  ]
  [ set color table:get colour-key "loss" ]

  set label (word
    farm-type ": "
    (length get-my-interventions) "/"
    (length get-my-interventions + length get-available-interventions))
end

to redraw-farms
  with-local-randomness [
    ask farms [ redraw-farm ]
  ]
end

to setup-colours
  set colour-key table:make
  table:put colour-key "farmer" black
  table:put colour-key "farmer-label" black
  table:put colour-key "farm-type"
    table:from-list zip ["SNB" "Dairy" "Forest" "Crop"] (list (orange - 2) (grey + 2) (green - 2) (yellow + 2))
  table:put colour-key "farm-type-palettes" (
    table:from-list zip ["SNB" "Dairy" "Forest" "Crop"] ["YlOrBr" "Greys" "PuBuGn" "YlGn"])
  table:put colour-key "owner-link" [0 0 0 180]
  table:put colour-key "border" red
  table:put colour-key "LUC-palette" "Greens"
  table:put colour-key "profit" grey - 3
  table:put colour-key "loss" [255 0 0 180]
  table:put colour-key "label" violet - 2
  table:put colour-key "background" grey
end

;; -----------------------------------------
;; reset code
;; -----------------------------------------

to store-initial-values
  ask farm-land [
    set landuse-0 landuse
  ]
  ask farms [
    set farm-type-0 farm-type
    set current-profit-0 current-profit
    set current-income-0 current-income
    set current-costs-0 current-costs
    ;; important to copy here to get a new matrix, not a reference to the old one
    set my-interventions-0 matrix:copy my-interventions
    set available-interventions-0 matrix:copy available-interventions
  ]
end

to restore-initial-values
  ask farm-land [
    set landuse landuse-0
  ]
  ask farms [
    set farm-type farm-type-0
    set current-profit current-profit-0
    set current-income current-income-0
    set current-costs current-costs-0
    ;; important to copy here to get a new matrix, not a reference to the old one
    set my-interventions matrix:copy my-interventions-0
    set available-interventions matrix:copy available-interventions-0
  ]
  redraw-farms
  reset-ticks
  tick
  if run-rng-seed != 0 [ random-seed run-rng-seed ]
end

;; -----------------------------------------
;; convenience reporting functions
;; -----------------------------------------

;; Convenience function for retrieving a parameter from one of
;; the global nested tables the values are stored in
to-report get-parameter [parameter mean-or-sd ft luc]
  if parameter = "yield" [
    report ifelse-value mean-or-sd = "mean"
    [ table:get table:get commodity-yield-means luc ft ]
    [ table:get table:get commodity-yield-sds luc ft ]
  ]
  if parameter = "cost" [
    report ifelse-value mean-or-sd = "mean"
    [ table:get table:get input-cost-means luc ft ]
    [ table:get table:get input-cost-sds luc ft ]
  ]
  if parameter = "emissions" [
    report ifelse-value mean-or-sd = "mean"
    [ table:get table:get ghg-emission-means luc ft ]
    [ table:get table:get ghg-emission-sds luc ft ]
  ]
  user-message (word "Incorrect request for parameter " parameter " " mean-or-sd)
  report nobody
end


;; -----------------------------------------
;; sigmoid function related
;; -----------------------------------------

;; see https://en.wikipedia.org/wiki/Sigmoid_function
;; the argument a increases the slope at (0, 0.5)
to-report sigmoid [x a]
  report 1 / (1 + exp (a * (- x)))
end

;; see https://en.wikipedia.org/wiki/Logit
;; inverse of the sigmoid function - used to determine
;; where on the sigmoid a given probability lies
;; NOTE: commented 'epsilon correction' avoids errors from
;; overflow/underflow but might be incorrect in practice
to-report logit [p a]
  if p = 0.5 [report 0]
  if p = 1 [report logit (1 - epsilon) a]
  if p = 0 [report logit epsilon a]
  report ln (p / (1 - p)) / a
end

;; reports revised probability from 'nudging' initial
;; initial probability p along a sigmoid curve
;; NOTE: that starting from 0 and 1 it's unclear if the epsilon
;; adjustment should be used or not... see commented out code
to-report nudged-probability [p nudge]
  ;; if p = 1 [
  ;;   report ifelse-value nudge > 0 [1] [nudged-probability (1 - epsilon) nudge]
  ;; ]
  ;; if p = 0 [
  ;;   report ifelse-value nudge < 0 [0] [nudged-probability epsilon nudge]
  ;; ]
  let centre logit p sigmoid-slope
  report sigmoid (centre + nudge) sigmoid-slope
end


;; -----------------------------------------
;; utilility functions
;; -----------------------------------------

;; -----------------------------------------
;; math stuff
;; -----------------------------------------
to-report rescale [x min-x max-x new-min-x new-max-x]
  report new-min-x + (new-max-x - new-min-x) * (x - min-x) / (max-x - min-x)
end

;; this formulation of relative change avoids overflows when
;; the base value is close to 0, and is symmetrical relative to the
;; direction of change ie the absolute value when x0 and x1 are
;; reversed will be the same, x0 and x1 must be the same sign.
to-report relative-change [x0 x1]
  if x0 = 0 and x1 = 0 [ report 0 ]
  if (x0 * x1) < 0 [
    user-message  "Error attempting to determine relative change between numbers of opposite sign"
    report nobody
  ]
  let change 2 * abs (x1 - x0) / (abs x1 + abs x0)
  ifelse x1 < x0
  [ report (- change) ]
  [ report    change  ]
end

to-report percent-change [a b]
  report 100 * relative-change a b
end

;; because the GIS extension makes NaNs and we need to detect them
to-report is-nan? [x]
  report not (x <= 0 or x >= 0)
end

;; -----------------------------------------
;; logic stuff
;; -----------------------------------------
to-report all-true? [lst]
  report is-boolean? position false lst
end

to-report any-true? [lst]
  report not is-boolean? position true lst
end

;; -----------------------------------------
;; matrix to lists and back stuff
;; -----------------------------------------
;; this is used by the emissions and yield impacts matrices
;; which assumulate MULTIPLICATIVELY
to-report matrix-column-products [m]
  report matrix:from-column-list (
    (list map [r -> product r] matrix:to-column-list m)
  )
end

to-report matrix-row-sums [m]
  report map [r -> sum r] matrix:to-row-list m
end

to-report matrix-column-sums [m]
  report map [c -> sum c] matrix:to-column-list m
end

to-report matrix-sum-elements [m]
  report sum matrix-row-sums m
end

to-report matrix-dup-rows [m n]
  if item 0 matrix:dimensions m != 1 [
    user-message "ERROR: Print matrix supplied to matrix-dup-rows reporter has more than one row"
    report nobody
  ]
  let row item 0 matrix:to-row-list m
  report matrix:from-row-list map [r -> row] range n
end

to-report matrix-dup-cols [m n]
  if item 1 matrix:dimensions m != 1 [
    user-message "ERROR: Print matrix supplied to matrix-dup-cols reporter has more than one column"
    report nobody
  ]
  let col item 0 matrix:to-column-list m
  report matrix:from-column-list map [c -> col] range n
end

to-report matrix-sum-of-rows-cols [row col]
  report matrix:plus matrix:from-row-list map [i -> row] range length col
                     matrix:from-column-list map [i -> col] range length row
end

to-report matrix-from-list [lst nr nc byrow?]
  ifelse byrow?
  [ report matrix:from-row-list as-list-of-lists lst nr ]
  [ report matrix:from-column-list as-list-of-lists lst nc ]
end

;; -----------------------------------------
;; list stuff
;; -----------------------------------------
;; zip two lists into a list of tuples (cf. python function)
to-report zip [l1 l2]
  report (map list l1 l2)
end

to-report product [numbers]
  ifelse length numbers = 0
  [ report 1 ]
  [ report reduce * numbers ]
end

to-report as-list-of-lists [lst n-sublists]
  let nm length lst
  let m nm / n-sublists
  let breaks sentence (n-values n-sublists [i -> i * m]) nm
  report (map [[a b] -> sublist lst a b] butlast breaks butfirst breaks)
end

to-report range-from-to-by [start finish step]
  let n ceiling ((finish - start) / step)
  if (step * (finish - start)) < 0 [
    user-message "***WARNING: step in sequence reporter is wrong sign. Returning empty list***"
    report []
  ]
  ifelse finish > start
  [ report n-values n [i -> start + step * i] ]
  [ report n-values n [i -> start + step * i] ]
end

to-report range-from-to [start finish]
  ifelse finish > start
  [ report range-from-to-by start finish 1 ]
  [ report range-from-to-by start finish -1 ]
end

to-report range-by [finish by]
  report range-from-to-by 0 finish by
end

to-report rep-list [lst n inline?]
  ifelse inline?
  [ report rep-list-inline lst n]
  [ report rep-list-each lst n]
end

to-report rep-list-each [lst n]
  report reduce [[a b] -> sentence a b ] map [i -> n-values n [j -> i] ] lst
end

to-report rep-list-inline [lst n]
  report reduce [[a b] -> sentence a b ] map [i -> lst] range n
end

to-report slice-by-ones [lst ones]
  report map [t -> item 0 t] filter [t -> item 1 t = 1] zip lst ones
end

;; ------------------------------------------
;; string stuff
;; ------------------------------------------
;; reports string into a list of characters
to-report string-as-list [str]
  report n-values length str [i -> item i str]
end

to-report join-string [lst sep]
  report reduce [[a b] -> (word a sep b)] lst
end

;; splits a string str on the separator provided
;; returns a list of strings
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


;; ------------------------------------------
;; drawing utilities (see netlogo-utils repo)
;; ------------------------------------------

to draw-borders [col]
  with-local-randomness [
    let boundaries farm-land with [
      any? neighbors4 with [the-owner != nobody and
                            the-owner != [the-owner] of myself]
    ]
    ask boundaries [
      ask neighbors4 with [the-owner != nobody] [
        ;; only those with different owner need to draw a line
        if the-owner != [the-owner] of myself [
          draw-line-between self myself col
        ]
      ]
    ]
  ]
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


to profile-setup
  profiler:start         ;; start profiling
  setup                  ;; set up the model
  profiler:stop          ;; stop profiling
  print profiler:report  ;; view the results
  profiler:reset         ;; clear the data
end


to profile-go [n]
  setup                  ;; set up the model
  profiler:start         ;; start profiling
  repeat n [go]
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
215
12
691
921
-1
-1
3.0
1
7
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
118
38
201
71
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
846
17
1089
50
sigmoid-slope
sigmoid-slope
0.01
5
2.5
0.01
1
NIL
HORIZONTAL

SWITCH
846
676
990
709
seed-setup-rng?
seed-setup-rng?
0
1
-1000

INPUTBOX
845
715
990
775
rng-economics
42.0
1
0
Number

OUTPUT
848
120
1056
208
11

TEXTBOX
852
99
1039
117
Interventions (for information)
12
0.0
1

SWITCH
845
224
1085
257
setup-geography-from-files?
setup-geography-from-files?
0
1
-1000

SLIDER
846
459
1073
492
luc-aggregation-steps
luc-aggregation-steps
0
250
100.0
5
1
NIL
HORIZONTAL

TEXTBOX
847
388
968
406
Random landscape
12
0.0
1

SLIDER
845
498
1132
531
inhibition-distance
inhibition-distance
0
sqrt (count patches / 100 / pi)
2.8
0.1
1
NIL
HORIZONTAL

SWITCH
39
661
203
694
show-luc-codes?
show-luc-codes?
0
1
-1000

TEXTBOX
843
656
998
674
Model process RNGs
12
0.0
1

BUTTON
128
738
202
771
redraw
redraw
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
119
137
200
170
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
119
218
199
251
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
850
305
988
350
region
region
"Rangitaiki"
0

BUTTON
53
780
202
813
toggle-labels
set show-labels? not show-labels?\nifelse show-labels?\n[ ask turtles \n  [ set label-color table:get colour-key \"label\" ] ]\n[ ask turtles\n  [ set label-color [0 0 0 0] ] ]\n
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
53
821
202
854
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

SLIDER
852
263
1024
296
max-dimension
max-dimension
100
600
300.0
10
1
NIL
HORIZONTAL

BUTTON
119
177
200
210
step-10
repeat 10 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
40
463
204
496
show-landuse?
show-landuse?
1
1
-1000

SWITCH
39
598
203
631
farm-type-colours?
farm-type-colours?
0
1
-1000

TEXTBOX
860
56
1075
86
Bigger numbers amplify the impact of nudges (in both directions)
11
0.0
1

TEXTBOX
854
354
1004
382
You'll need the named regional spatial subfolder
11
0.0
1

TEXTBOX
40
698
208
727
More intense colours are lower LUC values (better land).
11
0.0
1

TEXTBOX
43
500
195
584
Colour key (applies to both landuse and farm symbols)\n  SNB - Brown\n  Dairy - Grey\n  Forestry - Green \n  Crop - Yellow
11
0.0
1

TEXTBOX
40
635
208
653
Loss-making farms always red
11
0.0
1

TEXTBOX
1030
267
1119
328
Longer dimension of map will be this many patches.
11
0.0
1

INPUTBOX
847
580
948
640
rng-geography
0.0
1
0
Number

SWITCH
846
540
1066
573
seed-geography-rng?
seed-geography-rng?
0
1
-1000

BUTTON
26
37
89
70
reset
restore-initial-values\n
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
844
790
994
823
run-rng-seed
run-rng-seed
0
100
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
998
771
1121
824
Set to any value in experiments, but only small range provided for interactive use
11
0.0
1

TEXTBOX
846
828
1025
846
0 for non-seeded randomness
11
0.0
1

BUTTON
53
862
203
895
toggle-farms
ask farms [set hidden? not hidden?]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
37
310
203
343
show-interventions?
show-interventions?
1
1
-1000

TEXTBOX
44
346
194
374
Turn off messages to speed things up!
11
0.0
1

CHOOSER
846
408
1042
453
random-landscape-method
random-landscape-method
"voter-model" "averaging"
1

SLIDER
843
887
1015
920
carbon-price
carbon-price
1
50
25.0
1
1
NIL
HORIZONTAL

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

circle 3
false
0
Circle -7500403 true true 2 2 297
Circle -16777216 false false -1 -1 301

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

square 3
false
0
Rectangle -7500403 true true 0 0 300 300
Rectangle -16777216 false false 0 0 300 300

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
