__includes [
  "mtm-read-files.nls"
  "mtm-geography.nls"
  "mtm-utils.nls"
  "mtm-profile.nls"
  "mtm-render.nls"
]

extensions [
  csv      ;; easy reading of CSVs
  palette  ;; nicer colours
  table    ;; dictionary like tables for storing lots of the input parameters
  matrix   ;; matrix maths
  gis      ;; spatial data
  nw       ;; networks for cluster/connectivity detection
  rnd      ;; weighted random draws
  profiler ;; in case we need to figure out why things are slow
]

globals [
  ;; convenient to set these and forget them
  output-data-folder
  market-data-folder
  spatial-data-folder
  show-labels?

  epsilon                 ;; for convenience - constant for 'smallest number'
  na-value                ;; matrices can only store numbers, unlike tables, so we need a sentinel
                          ;; NA value. In time-honoured GIS tradition it will get set to -999

  parcels-data            ;; GIS functionality depends on reading the data into an object
  luc-data                ;; then acccessing it again later, so these do that for parcels and LUC

  farm-land               ;; patches that are on farms
  not-farm-land           ;; and patches that are not - esp. important for GIS data

  farm-types              ;; list of named farm types - likely fixed at 4: Crop Dairy Forestry SNB
  intervention-types      ;; list of named interventions - more likely to change over time
  interventions           ;; table of tables of intervention impacts

  ;; model parameters are read into tables as follows, but...
  commodity-yield-means   ;; table of mean yields by LUC and farm-type
  commodity-yield-sds     ;; table of sd of yields by LUC and farm-type
  input-cost-means        ;; table of mean input costs by LUC and farm-type
  input-cost-sds          ;; table of sd of input costs by LUC and farm-type
  ghg-emission-means      ;; table of mean GHG emissions by LUC and farm-type
  ghg-emission-sds        ;; table of sd of GHG emissions by LUC and farm-type
  prices                  ;; table of commodity prices
  base-thresholds         ;; table of default farmer decision thresholds for various interventions

  ;; ... matrix equivalents of the above tables, which have priority in running the model
  ;; it may, in time make sense to drop the tables entirely, although they are more self-documenting
  ;; than the matrices where there is no information in the row-column ordering about what each
  ;; refers to

  ;; First 6 matrices have rows-cols entries for LUC - farm-type respectively
  ;; rows order LUC1 to LUC8, cols ordered alphabetically by farm-type (per the list farm-types)
  m-yield-means
  m-yield-sds
  m-cost-means
  m-cost-sds
  m-emission-means
  m-emission-sds
  ;; a column matrix ordered by farm-types list
  m-prices
  ;; rows-cols entries for intervention-type - farm-type respectively
  ;; ordered by intervention-types and farm-types lists
  m-intervention-cost-impacts
  m-intervention-yield-impacts
  m-intervention-emissions-impacts

  ;; colour settings - these can be changed in one place, see setup-key-colours procedure
  colour-key              ;; table of colour settings
]

breed [farmers farmer]    ;; one farmer per farm
breed [farms farm]        ;; turtle where most of the action happens
breed [nodes node]        ;; a utility turtle type mostly used for cluster detection using nw extension

farms-own [
  my-farmer               ;; farmer who owns/runs farm
  farm-type               ;; farm type of farm - one of farm-types global
  the-land                ;; patch-set of patches on farm
  landuse-luc-profile     ;; matrix of counts of LUC - farm-type (i.e. landuse) combinations of the-land
  current-profit          ;; most recent profit of farm summed across patches
  current-income          ;; most recent income of farm summed across patches
  current-costs           ;; most recent costs of farm summed across patches
  my-interventions        ;; row matrix of 0/1s indicating adoption of intervention-types
  available-interventions ;; row matrix of 0/1s indicating interventions still open contingent on farm-type
  ;; below are initial values of some of the above to allow easy reset to initial state
  farm-type-0
  landuse-luc-profile-0
  current-profit-0
  current-income-0
  current-costs-0
  my-interventions-0
  available-interventions-0
]

farmers-own [
  my-farm                 ;; farm turtle of this farmer's farm
]

patches-own [
  temp-id                 ;; used during setup to assign to farmers / assemble farms
  region-id               ;; generic variable for labelling any time we are clustering/regionalising patches
  farm-id                 ;; farm-id supercedes temp-id after clean up of empty farms etc. during set up
                          ;; NOTE: farm-id is not the same as [who] of the-owner farmer
  sub-farm-id             ;; sub farm level id partitioning farms into areas for possible landuse conversion
  the-owner               ;; farmer who owns this patch
  luc-code                ;; LUC code where 1 = LUC1, 2 = LUC2, etc. (1 is best, 8 is worst)
  landuse                 ;; one of global farm-types - initially will match farm but may change over time
  landuse-0               ;; for reset to initial state
]


;; -----------------------------------------
;; MAIN LOOP i.e., setup and go
;; -----------------------------------------
to setup
  ;; Setup order is very sensitive to a number of interdependencies among the various
  ;; elements. So be VERY CAREFUL IF CHANGING THE SEQUENCE OF OPERATIONS in this procedure
  clear-all

  set epsilon 1e-16
  set na-value -999
  set show-labels? false

  let base-data-folder "data/"
  set output-data-folder word base-data-folder "output/"                ;; 'data/output/'
  set market-data-folder word base-data-folder "market/"                ;; 'data/market/'
  set spatial-data-folder (word base-data-folder "spatial/" region "/") ;; 'data/spatial/REGION-NAME/'

  ;; this really has to come first (and note that it will read the GIS data if in that mode
  setup-world-dimensions

  setup-farmer-parameters
  setup-colours ;; must come after reading farmer parameters since it depends on farm types
  ask patches [ set pcolor table:get colour-key "background" ]

  setup-geography
  setup-economic-parameters
  redraw
  reset-ticks
  if run-rng-seed != 0 [ random-seed run-rng-seed ]
  go ;; this initialises the farms with current net profit and some interventions
  store-initial-values
  ;; because set up involves a full go step, we set the seed again to
  ;; allow repeatability if model is restored to that initial state
  if run-rng-seed != 0 [ random-seed run-rng-seed ]
end

;; core model loop
to go
  ifelse stop-model? [
    cleanup
    stop
  ]
  [
    ;; update farm status
    ask farms [
      update-profit-of-farm
    ]
    ask farmers [
      if [length available-interventions-as-list] of my-farm > 0 [
        ;; this reports a [intervention-type probability] pair
        let potential-change consider-interventions false
        if random-float 1 < last potential-change [
          if show-events? [
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

;; any model stop condition goes here
to-report stop-model?
  ifelse all-true? [length available-interventions-as-list = 0] of farms [
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
;; farm specific functions
;; -----------------------------------------

;; farm 'constructor'
to initialise-farm
  ;; will be called by the farmer who is 'myself' in this context
  set my-farmer myself
  set the-land farm-land with [the-owner = [my-farmer] of myself]
  ifelse not any? the-land [
    ;; a null farm (which can happen due to random placement of farmers
    ;; or parcels in GIS data below resolution of the patch grid
    ask my-farmer [ die ]
    die
  ]
  [
    setxy mean [pxcor] of the-land mean [pycor] of the-land
    set shape "square 3"
    set hidden? false
    set farm-type one-of farm-types
    ask the-land [ set landuse [farm-type] of myself ]
    set label farm-type
    set label-color table:get colour-key "label"
    set label-color ifelse-value show-labels? [table:get colour-key "label"] [[0 0 0 0]]
    ;; interventions are a 1 row matrix of 0s to start
    set my-interventions matrix:make-constant 1 (length intervention-types) 0
    set available-interventions possible-interventions
  ]
end

;; for this farm's type returns 0/1 row matrix indicating which are possible on this type of farm
to-report possible-interventions
  report row-matrix map [i -> ifelse-value is-applicable-to-farm-type? i [1] [0]] intervention-types
end

;; update my-interventions and available-interventions matrices
;; to reflect implementation of intervention i-type
to implement-intervention [i-type]
  let col position i-type intervention-types
  matrix:set my-interventions 0 col 1
  matrix:set available-interventions 0 col 0
end

;; this always applies year-on-year variance so no 'with-var?' parameter required
to update-profit-of-farm
  set current-income get-farm-income true
  set current-costs get-farm-costs true
  set current-profit current-income - current-costs
  redraw-farm
end

;; reports my-interventions as a list of intervention-type names for convenience
;; farm reporter
to-report my-interventions-as-list
  ;; slice by ones returns items in the first list that match 1s in the second
  ;; so e.g. ["A" "B" "C" "D"] [0 1 1 0] -> ["B" "C"]
  report slice-by-ones intervention-types (matrix:get-row my-interventions 0)
end

;; reports available-interventions as a list of intervention-type names for convenience
;; farm reporter
to-report available-interventions-as-list
  report slice-by-ones intervention-types (matrix:get-row available-interventions 0)
end

;; reports a column matrix from the column sums of current landuse-luc-profile matrix
;; farm reporter
to-report get-farm-landuse-profile
  report matrix:from-column-list (list matrix-column-sums landuse-luc-profile)
end

;; reports a column matrix from the row sums of current landuse-luc-profile matrix
;; farm reporter
to-report get-farm-luc-profile
  report matrix:from-column-list (list matrix-row-sums landuse-luc-profile)
end

;; tests if intervention is applicable by checking if the result is in interventions table
;; farm reporter
to-report is-applicable-to-farm-type? [i-type]
  report get-impact farm-type "costs" i-type != na-value
end

;; scores a potential intervention by comparing expected income and costs
;; were it to be implemented relative to current income and costs
;; TODO: consider if this actually makes sense... in a good year when current income was high
;; and current costs low the score might be lower which is slightly counter-intuitive.
;; farm reporter because the cost and income reporters invoked are farm reporters
to-report get-intervention-score [i-type show-messages?]
  let col (position i-type intervention-types)
  matrix:set my-interventions 0 col 1
  ;; get long-term expect costs and incomes, i.e. without variance
  let new-costs get-farm-costs false
  let new-income get-farm-income false
  let delta-costs relative-change current-costs new-costs
  let delta-income relative-change current-income new-income
  if show-messages? [
    show i-type
    show (word "Current costs: " current-costs " Current income: " current-income)
    show (word "New costs    : " new-costs     " New income    : " new-income)
    show (word "Delta costs  : " delta-costs   " Delta income  : " delta-income)
  ]
  ;; undo the intervention... we are only trying them on for size here!
  matrix:set my-interventions 0 col 0
  report delta-income - delta-costs
end

;; reports a 8 row 4 col matrix of the count of patches with each combination of
;; LUC code and landuse/farm-type in the patch-set poly. Usually this is called by a
;; farm, when poly will be 'the-land'. Initially all patches will be the same landuse
;; matching the farm-type, but some farmer actions may lead to this changing.
;; Run during initialisation and only altered if such changes in landuse are
;; undertaken.
;; not a farm reporter but usually run on the-land of farm turtles
to-report get-farm-landuse-luc-profile [poly]
  let luc-i [position luc-code range-from-to 1 9] of poly
  let lu-j [position landuse farm-types] of poly
  let nrow 8
  let ncol length farm-types
  let farm-profile matrix:make-constant nrow ncol 0
  (foreach luc-i lu-j [ [i j] ->
    matrix:set farm-profile i j matrix:get farm-profile i j + 1
  ])
  report farm-profile
end

;; we might need some more of these...
;; returns impact by farm-type, effect-type, and intervention-type
;; not a farm reporter but most often run by a farm turtle
to-report get-impact [f-type e-type i-type]
  report table:get (table:get (table:get interventions i-type) e-type) f-type
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
end

;; returns threshold for the supplied intervention
;; this or a wrapper for this could include farmer demography 'shifts'
;; or social/geographic network shifts
;; farmer reporter due to dependency on my-farm attribute
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
  let to-consider [available-interventions-as-list] of fm
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
;; matrix calculations of farm income etc.
;; -----------------------------------------
;; most of these are farm reporters because they rely on the luc-landuse-profile matrix
;; which it would be inefficient to reassemble on the fly and which is rarely updated anyway

;; farm reporter due to landuse-luc-profile
to-report get-farm-income [with-var?]
  ;; calculation here is SUM [ LUC/LU * Yield * Impact of interventions * Prices ]
  ;; Yield component optionally includes variance
  ;; Multiplications are all element-wise except for last step * Prices
  ifelse with-var? [
    report round matrix-sum-elements (
      ( matrix:map [[a b c] -> a * b * c]
        landuse-luc-profile
        matrix:plus m-yield-means get-farm-yield-variance
        get-yield-after-interventions ) matrix:* m-prices )
  ]
  [ report round matrix-sum-elements (
      ( matrix:map [[a b c] -> a * b * c]
        landuse-luc-profile
        m-yield-means  ;; no variance
        get-yield-after-interventions ) matrix:* m-prices )
  ]
end

;; farm reporter due to landuse-luc-profile
to-report get-farm-costs [with-var?]
  ;; calculation here is
  ;; Cost of interventions + SUM [ LUC/LU * (Costs + (Emissions * Impact of interventions) * tax) ]
  ;; Emissions optionally includes variance
  ;; Multiplications are all element-wise except for last step * Prices
  ifelse with-var? [
    report round ( get-cost-after-interventions + matrix-sum-elements
      ( matrix:map *
        landuse-luc-profile
        ( m-cost-means matrix:+
          get-farm-cost-variance matrix:+
          ( matrix:map *
              ;; here with variance
              ( matrix:plus m-emission-means get-farm-emissions-variance )
              get-emissions-after-interventions ) matrix:* carbon-price
        )
      )
    )
  ]
  [ report round ( get-cost-after-interventions + matrix-sum-elements
      ( matrix:map *
        landuse-luc-profile
        ( m-cost-means matrix:+
          ( matrix:map *
              m-emission-means ;; no variance
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
;; so the overall deviation will be larger, but appropriately 'suppressed'
;; for the larger holding. This also allows us to avoid div 0 errors...
to-report reduced-normal-deviate [m s n]
  (ifelse
    n = 0 [ report 0 ] ;; n = 0 is meaningless
    n = 1 [ report random-normal m s ]
    [ report random-normal m (s / sqrt(n)) ])
end

;; reports a matrix of yield deviates suppressed by the number of patches
;; farm reporter due to landuse-luc-profile
to-report get-farm-yield-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-yield-sds landuse-luc-profile)
end

;; reports a matrix of cost deviates suppressed by the number of patches
;; farm reporter due to landuse-luc-profile
to-report get-farm-cost-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-cost-sds landuse-luc-profile)
end

;; reports a matrix of emissions deviate suppressed by the number of patches
;; farm reporter due to landuse-luc-profile
to-report get-farm-emissions-variance
  report (matrix:map [[s n] -> reduced-normal-deviate 0 s n] m-emission-sds landuse-luc-profile)
end

;; farm reporter due to get-farm-landuse-profile
to-report get-cost-after-interventions
  ;; the calculation here is
  ;;                       [       costs        ]   [   col-matrix    ]
  ;; [interventions 0/1] * [ rows: intervention ] * [                 ]
  ;;                       [ cols: farm-type    ]   [ rows: farm-type ]
  ;;
  report round matrix-sum-elements
         ( my-interventions matrix:*
           m-intervention-cost-impacts matrix:*
           get-farm-landuse-profile )
end

;; reports a matrix for use in multiplication of yields dependent on on-farm interventions.
;; This is a non-standard matrix operation where desired result is the cumulative product of
;; each column in the matrix but not all row entries in the matrix are applicable (because not a
;; all interventions will have been adopted).
;;
;; farm reporter due to my-interventions
to-report get-yield-after-interventions
  report matrix:transpose
           matrix-duplicate-cols (
             matrix-column-products ( ;; cumulative product of adopted impacts
             ( matrix:map [[a b] -> ifelse-value a = 0 [1] [b]] ;; map non-adopted interventions to 1
               matrix-duplicate-cols matrix:transpose my-interventions length farm-types ;; dup for each farm-type
               m-intervention-yield-impacts )
             )
           ) 8 ;; duplicated for each LUC
end

;; as above but for emissions
;;
;; farm reporter due to my-interventions
to-report get-emissions-after-interventions
  report matrix:transpose
           matrix-duplicate-cols (
             matrix-column-products (
             ( matrix:map [[a b] -> ifelse-value a = 0 [1] [b]]
               matrix-duplicate-cols matrix:transpose my-interventions length farm-types
               m-intervention-emissions-impacts )
             )
           ) 8
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
;; The 'epsilon correction' for 0 and 1 avoids errors from
;; overflow/underflow even if introduces slight errors
;; but hey! nothing is impossible, nor a sure thing!
to-report logit [p a]
  (ifelse
    p = 0.5 [ report 0 ]
    p = 1 [ report logit (1 - epsilon) a ]
    p = 0 [ report logit epsilon a ]
    [ report ln (p / (1 - p)) / a ])
end

;; reports revised probability from 'nudging' initial
;; initial probability p along a sigmoid curve
to-report nudged-probability [p nudge]
  let centre logit p sigmoid-slope
  report sigmoid (centre + nudge) sigmoid-slope
end


;; The MIT License (MIT)
;;
;; Copyright (c) 2023-24 David O'Sullivan
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
847
679
991
712
seed-setup-rng?
seed-setup-rng?
0
1
-1000

INPUTBOX
846
718
991
778
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
150.0
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
844
659
999
677
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
848
264
1020
297
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
Colour key (applies to both landuse and farm symbols)\n  SNB - Brown\n  Dairy - Grey\n  Forestry - Blue-Green \n  Crop - Yellow-Green
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
1026
265
1115
326
Longer dimension of map will be this many patches.
11
0.0
1

INPUTBOX
848
585
949
645
rng-geography
0.0
1
0
Number

SWITCH
847
543
1067
576
seed-geography-rng?
seed-geography-rng?
1
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
845
793
995
826
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
999
774
1122
827
Set to any value in experiments, but only small range provided for interactive use
11
0.0
1

TEXTBOX
847
831
1026
849
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
show-events?
show-events?
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

SLIDER
845
500
1017
533
number-of-farms
number-of-farms
10
1000
250.0
5
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
