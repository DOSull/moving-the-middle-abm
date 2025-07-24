__includes [
  "mtm-read-files.nls"     ;; reads the data files pertaining to markets, policy settings etc
  "mtm-geography.nls"      ;; reads geospatial data or initialises a random landscape
  "mtm-utils.nls"          ;; a range of useful maths, list, string, matrix etc. utilities
  "mtm-render.nls"         ;; code to draw the map
  "mtm-farmer.nls"         ;; code for farmer turtles
  "mtm-farm.nls"           ;; code for farm turtles
  "mtm-holding.nls"        ;; code for holding turtles
  "mtm-plots.nls"          ;; plotting code allowing for varying numbers of pens
  "mtm-results.nls"
  "distribution-utils.nls" ;; generic statistical function
  "list-utils.nls"         ;; generic list utilities
  "mtm-profile.nls"        ;; profiling stuff goes here
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
  pathdir
  ;; sr
  ;; xw
]

globals [
  ;; convenient to set these and forget them
  output-data-folder
  market-data-folder
  spatial-data-folder
  region
  scenario
  results

  show-labels?            ;; generally don't want to see labels on turtles

  epsilon                 ;; for convenience - constant for 'smallest number'
  na-value                ;; matrices can only store numbers, unlike tables, so we need a sentinel
                          ;; NA value. In time-honoured GIS tradition it will be set to -999

  geography-seed
  run-seed

  parcels-data            ;; GIS functionality depends on reading the data into an object
  luc-data                ;; then acccessing it again later, so these do that for parcels, LUC,
  landuse-data            ;; and landuse
  parcels-raster

  farm-land               ;; patches that are on farms
  not-farm-land           ;; and patches that are not - esp. important for GIS data

  farm-types              ;; list of named farm types - likely fixed at 4: Crop Dairy Forestry SNB
  mgmt-intervention-types ;; list of named management interventions - more likely to change over time
  dispositions            ;; list of farmer dispositions

  ;; some model parameters are read into tables for relative ease of retrieval
  farm-type-suitabilities ;; table of LUC levels suitable for each farm-type
  farm-type-change-probs  ;; table of probability of conversion between farm types
  env-metrics             ;; table of environmental metrics with policy limits on per ha. emissions

  ;; these tables are only used in setup - the data are passed into matrix copies for use in calculations
  mgmt-interventions      ;; table of tables of management intervention impacts
  base-thresholds         ;; table of default farmer decision thresholds for various interventions

  ;; data stored as matrices for use in calculations
  ;; First 6 matrices have rows - cols entries for LUC - farm-type respectively
  ;; rows order LUC1 to LUC8, cols ordered alphabetically by farm-type
  ;; Note that the table env-metrics-means and env-metrics-sds contain similar
  ;; matrices in a table indexed by the metric name
  cost-means
  cost-sds
  yield-means
  yield-sds
  env-metric-means        ;; table of matrices of means of environmental metrics by LUC (row) and farm-type (col)
  env-metric-sds          ;; table of matrices of sd of environmental metrics by LUC (row) and farm-type (col)
  prices                  ;; column matrix of product prices (per ha) ordered by farm-types list

  ;; matrices with rows-cols entries for intervention-type - farm-type respectively
  ;; ordered by intervention-types and farm-types lists
  intervention-cost-impacts
  intervention-yield-impacts
  intervention-env-impacts ;; table of intervention impact matrices indexed by 'ghg-emissions', 'nitrates' etc.

  ;; colour settings - these can be changed in one place, see mtm-render::setup-key-colours procedure
  colour-key              ;; table of colour settings
]

undirected-link-breed [catchment-links catchment-link]
undirected-link-breed [local-links local-link]

breed [holdings holding]  ;; where the action happens (profit calculated, landuse changed, interventions applied)
                          ;; a farm consists of one or more holdings
breed [farms farm]        ;; convenient aggregator of holdings information
breed [farmers farmer]    ;; one farmer per farm
breed [nodes node]        ;; a utility turtle type mostly used for cluster detection using nw extension

patches-own [
  temp-id                 ;; used during setup to assign to farmers / assemble farms
  region-id               ;; generic variable for labelling any time we are clustering/regionalising patches
  farm-id                 ;; farm-id supercedes temp-id after clean up of empty farms etc. during set up
                          ;; NOTE: farm-id is not the same as [who] of the-owner farmer
  holding-id              ;; sub farm level id partitioning farms into areas for possible landuse conversion
  the-owner               ;; farmer who owns this patch
  luc-code                ;; LUC code where 1 = LUC1, 2 = LUC2, etc. (1 is best, 8 is worst)
  landuse                 ;; one of global farm-types - initially will match farm but may change over time
  landuse-0               ;; for reset to initial state
]


;; ----------------------------------------------------------------------------
;; MAIN LOOP i.e., setup and go
;; ----------------------------------------------------------------------------
to setup
  ;; Setup order is very sensitive to a number of interdependencies among the various
  ;; elements. So be VERY CAREFUL IF CHANGING THE SEQUENCE OF OPERATIONS in this procedure
  ifelse reinitialise? or geography-from-files? [
    clear-all
    setup-folders  ;; mtm-read-files.nls
  ]
  [
    clear-turtles
    clear-patches
    clear-all-plots
    clear-drawing
    clear-ticks
    setup-world-dimensions
  ]
  set epsilon 1e-16
  set na-value -999
  set show-labels? false

  set geography-seed ifelse-value user-geography-seed? [geography-rng-seed] [new-seed]
  set run-seed ifelse-value user-run-seed? [run-rng-seed] [new-seed]

  if reinitialise? or geography-from-files? [
    carefully [
      if geography-from-files? [
        set region user-one-of "Set the spatial setting" sort filter [d -> first d != "."] pathdir:list spatial-data-folder
        set spatial-data-folder join-list (list spatial-data-folder region) pathdir:get-separator
      ]
      setup-world-dimensions
      set scenario user-one-of "Set the market scenario" sort filter [d -> first d != "."] pathdir:list market-data-folder
      set market-data-folder join-list (list market-data-folder scenario) pathdir:get-separator

      setup-farmer-parameters     ;; mtm-read-files.nls
      ;; if we figure out a use for dispositions (Kaine et al. argue against them!) then these
      ;; will likely be read in from files too, but for now:
      set dispositions [ "for-profit" "pro-social" "pro-environmental" ]
      set reinitialise? false
    ]
    [
      user-message "An initialisation error occurred. Check that you picked geography and scenario folders that contain all necessary files."
      stop
    ]
  ]
  setup-colours ;; must come AFTER reading farmer parameters since it depends on farm types
  ask patches [ set pcolor table:get colour-key "background" ]
  set show-local-links? include-networks?

  carefully [
    setup-geography                  ;; mtm-geography.nls
    setup-economic-parameters        ;; mtm-read-files.nls
    make-matrix-copies-of-some-data  ;; mtm-read-files.nls
  ]
  [
    user-message "Have you initialised the model correctly? Set the initialised? switch On and try setup again ensuring you pick a folder with a complete set of initialisation .csv files."
  ]
  setup-results-tables        ;; mtm-results.nls
  reset-ticks
  random-seed 0 ;; make the burn-in tick identical regardless of seed settings
  go ;; this initialises the farms with current net profit and some interventions
  store-initial-values   ;; store
  setup-model-plots      ;; mtm-plots.nls
  restore-initial-values ;; restore to that resetting is exactly the same
;  restore-initial-values ;; restore to that resetting is exactly the same
end

;; core model loop
to go
  ifelse not force? and stop-model? [
    cleanup
    stop
  ]
  [
    print (word "------------------------------ tick " ticks " ------------------------------")
    ;; update farm status - note that succession may also lead to change of farm type
    ask farmers [ age-and-succeed-farmer ]
    ask holdings [
      update-environmental-metrics-of-holding
      update-profit-of-holding
    ]
    ask farms [
      update-environmental-metrics-of-farm
      update-profit-of-farm
    ]
    ask farmers [
      let ft-now farm-type
      let farm-conversion-options []
      let loss-making-holdings get-loss-making-holdings
      ( ;; first we check if there are any holdings losing money
        ifelse any? loss-making-holdings [
          if [current-profit] of my-farm < 0 [ ;; losing lots of money so consider complete change
            let down-weight ifelse-value apply-severity-of-losses?
              [ 1 - (sum [count the-land] of loss-making-holdings) / count the-land ] [ 1 ]
            set farm-conversion-options consider-farm-type-change down-weight "Losing money: " false
          ]
          ;; only some holdings are losing money so consider forestry on those
          let holding-change-options consider-holdings-farm-type-change loss-making-holdings
                prioritise-forestry? "Some holdings losing money: " false
          make-farm-type-changes (sentence farm-conversion-options holding-change-options)
        ]
        ;;
        ;; further consider landuse changes if any holdings breach environmental regs
        ;;
        any-true? map [k -> [in-breach? k] of my-farm] table:keys env-metrics [
          let breached-metrics get-breached-metrics
          let holdings-in-breach get-holdings-in-breach breached-metrics
          let holding-change-options consider-holdings-farm-type-change holdings-in-breach
                prioritise-forestry? "Some holdings in breach of regulations: " false
          make-farm-type-changes (sentence farm-conversion-options holding-change-options)
        ]
        ;;
        ;; we're doing OK so think about management interventions instead
        ;;
        [
          if any? my-holdings with [any-interventions-to-do?] [
            ;; this reports a [holding [intervention-type probability]] list
            let potential-changes consider-management-changes "Management change: " false
            ;; which we pass to make-management-changes
            if length potential-changes > 0 [ make-management-changes potential-changes ]
          ]
        ]
      )
      set color ifelse-value farm-type = ft-now [ black ] [ red ]
    ]
    ask farms [redraw-farm]
    ask holdings [redraw-holding]
    if ticks > 0 [
      update-model-plots true
    ]
    update-model-results
    tick
  ]
end

;; any desired model stop condition goes here
to-report stop-model?
  ifelse any? holdings with [any-interventions-to-do?] [
    report false
  ]
  [
    show "Stopping model: all possible interventions implemented on all holdings!"
    report true
  ]
end

;; put any model clean up at end here
to cleanup
  ;; any end of run tidying up
end


;; ----------------------------------------------------------------------------
;; reset code
;; these procedures store the initial state
;; of the model to allow quick reset
;;
;; DON'T FORGET TO UPDATE IF NEW VARIABLES ARE ADDED!
;; ----------------------------------------------------------------------------

to store-initial-values
  ask farm-land [ set landuse-0 landuse ]
  ask holdings [
    set farm-type-0 farm-type
    set current-profit-0 current-profit
    set current-income-0 current-income
    set current-costs-0 current-costs
    ;; important to COPY here to get a new matrix, not a reference to the old one
    set my-interventions-0 matrix:copy my-interventions
    set avail-interventions-0 matrix:copy avail-interventions
    set landuse-luc-profile-0 matrix:copy landuse-luc-profile
    set my-metrics-0 table:from-json table:to-json my-metrics
  ]
  ask farms [
    set farm-type-0 farm-type
    set current-profit-0 current-profit
    set current-income-0 current-income
    set current-costs-0 current-costs
    set losses-record-0 losses-record
    set my-metrics-0 table:from-json table:to-json my-metrics
  ]
  ask farmers [
    set farm-type-0 farm-type
    set disposition-0 disposition
    set age-0 age
    set succession-stage-0 succession-stage
    set my-interventions-0 matrix:copy my-interventions
  ]
end

to restore-initial-values
  ask farm-land [ set landuse landuse-0 ]
  ask holdings [
    set farm-type farm-type-0
    set current-profit current-profit-0
    set current-income current-income-0
    set current-costs current-costs-0
    ;; important to COPY here to get a new matrix, not a reference to the old one
    set my-interventions matrix:copy my-interventions-0
    set avail-interventions matrix:copy avail-interventions-0
    set landuse-luc-profile matrix:copy landuse-luc-profile-0
    set my-metrics table:from-json table:to-json my-metrics-0
  ]
  ask farms [
    set farm-type farm-type-0
    set current-profit current-profit-0
    set current-income current-income-0
    set current-costs current-costs-0
    set losses-record losses-record-0
    set my-metrics table:from-json table:to-json my-metrics-0
  ]
  ask farmers [
    set farm-type farm-type-0
    set disposition disposition-0
    set age age-0
    set succession-stage succession-stage-0
    set my-interventions matrix:copy my-interventions-0
  ]
  redraw-farms-and-holdings
  update-model-plots false
  update-model-results
  reset-results-tables
  reset-ticks
  tick
  reset-rng
;  set run-seed ifelse-value user-run-seed? [run-rng-seed] [new-seed]
;  random-seed run-seed
end

to reset-rng
  set run-seed ifelse-value user-run-seed? [run-rng-seed] [new-seed]
  random-seed run-seed
end

;; The MIT License (MIT)
;;
;; Copyright (c) 2023-25 David O'Sullivan
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
201
10
818
922
-1
-1
3.0
1
6
1
1
1
0
0
0
1
0
202
0
300
1
1
1
Year
30.0

SWITCH
10
10
190
43
include-networks?
include-networks?
1
1
-1000

SWITCH
10
50
190
83
reinitialise?
reinitialise?
1
1
-1000

BUTTON
10
90
100
123
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
0

BUTTON
110
90
190
123
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

BUTTON
10
130
100
163
NIL
reset-rng
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
110
130
190
163
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

SWITCH
10
170
100
203
force?
force?
0
1
-1000

BUTTON
110
170
190
203
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

TEXTBOX
10
205
95
235
Go even if stop condition met
11
0.0
1

BUTTON
110
210
190
243
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

SWITCH
10
250
190
283
show-events?
show-events?
1
1
-1000

TEXTBOX
10
285
190
315
Turn off messages to speed things up!
11
0.0
1

SWITCH
10
320
190
353
show-landuse?
show-landuse?
0
1
-1000

TEXTBOX
10
355
190
395
Colour key (applies to both landuse and farm symbols)\n  Crop: Purple       Dairy: Blue\n  Forestry: Green  SNB: Brown
11
0.0
1

SWITCH
10
425
190
458
farm-type-colours?
farm-type-colours?
0
1
-1000

TEXTBOX
10
460
190
480
Loss-making farms always red
11
0.0
1

SWITCH
10
485
190
518
show-luc-codes?
show-luc-codes?
0
1
-1000

TEXTBOX
10
520
190
550
More intense colours are lower LUC values (better land).
11
0.0
1

BUTTON
10
560
190
593
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
10
600
190
633
toggle-farmers
ask farmers [ set hidden? not hidden? ]
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
10
640
190
673
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

BUTTON
10
680
190
713
toggle-holdings
ask holdings [set hidden? not hidden?]
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
10
730
190
763
show-local-links?
show-local-links?
1
1
-1000

SWITCH
10
770
190
803
show-catchment-links?
show-catchment-links?
1
1
-1000

SLIDER
10
810
190
843
rel-weight-locals
rel-weight-locals
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
10
850
190
883
rel-weight-catchment
rel-weight-catchment
0
1
0.1
0.01
1
NIL
HORIZONTAL

SWITCH
830
10
1010
43
geography-from-files?
geography-from-files?
1
1
-1000

SLIDER
830
50
1010
83
max-dimension
max-dimension
100
600
301.0
1
1
NIL
HORIZONTAL

TEXTBOX
830
85
1010
115
Longer dimension of map will be this many patches
11
0.0
1

TEXTBOX
830
120
1010
138
Random landscape
12
0.0
1

INPUTBOX
830
140
1010
200
geography-rng-seed
1.553817984E9
1
0
Number

SWITCH
830
205
1010
238
user-geography-seed?
user-geography-seed?
0
1
-1000

MONITOR
830
245
1010
290
NIL
geography-seed
0
1
11

CHOOSER
830
300
1010
345
random-landscape-method
random-landscape-method
"voter-model" "averaging"
1

SLIDER
830
350
1010
383
luc-aggregation-steps
luc-aggregation-steps
0
250
90.0
5
1
NIL
HORIZONTAL

SLIDER
830
390
1010
423
number-of-farms
number-of-farms
10
1000
250.0
5
1
NIL
HORIZONTAL

SWITCH
830
430
1010
463
correlated-landuse?
correlated-landuse?
0
1
-1000

SLIDER
830
470
1010
503
landuse-aggregation-steps
landuse-aggregation-steps
0
20
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
830
510
1010
528
Model run RNG
12
0.0
1

INPUTBOX
830
530
1010
590
run-rng-seed
54.0
1
0
Number

SWITCH
830
595
1010
628
user-run-seed?
user-run-seed?
1
1
-1000

MONITOR
830
635
1010
680
NIL
run-seed
0
1
11

SLIDER
830
690
1010
723
sigmoid-slope
sigmoid-slope
0.01
20
10.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
830
740
1010
758
Interventions (for information)
12
0.0
1

OUTPUT
830
760
1010
920
11

INPUTBOX
1020
10
1240
70
experiment-name
scratch
1
0
String

BUTTON
1250
10
1420
43
output-results
output-results
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1250
45
1430
88
experiment-name should be an existing subfolder of the output folder
11
0.0
1

PLOT
1020
90
1420
220
Income-costs
Time
$ Total
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
1020
230
1420
360
Interventions
Time
# Cells
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
1020
370
1420
500
Landuse composition
Time
# Cells
0.0
10.0
0.0
10.0
true
true
"" ""
PENS

PLOT
1020
510
1420
640
Environmental metrics
Time
NIL
0.0
0.0
0.0
1.0
true
true
"" ""
PENS

SWITCH
1020
650
1320
683
landuse-change-on-succession?
landuse-change-on-succession?
0
1
-1000

SWITCH
1020
690
1220
723
prioritise-forestry?
prioritise-forestry?
0
1
-1000

TEXTBOX
1225
694
1370
719
Off: Consider all landuses for loss-making holdings
10
0.0
1

SWITCH
1020
730
1220
763
apply-severity-of-losses?
apply-severity-of-losses?
1
1
-1000

TEXTBOX
1225
734
1370
759
On: Severe losses make farm conversion more likely
10
0.0
1

SWITCH
1020
770
1220
803
apply-suitability?
apply-suitability?
0
1
-1000

TEXTBOX
1225
774
1370
799
On: LUC suitability applied to farm change
10
0.0
1

TEXTBOX
1020
820
1220
838
--------- NOT YET IN USE ---------
11
15.0
1

SLIDER
1020
840
1220
873
bad-years-trigger
bad-years-trigger
0
1
1.0
0.001
1
NIL
HORIZONTAL

TEXTBOX
1225
844
1370
869
Proportion of recent years of losses to trigger conversion 
10
0.0
1

SLIDER
1020
880
1220
913
years-to-remember
years-to-remember
0
25
10.0
1
1
NIL
HORIZONTAL

TEXTBOX
1225
884
1370
909
Length of memory of losses
10
0.0
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

circle 3
false
0
Circle -7500403 true true 0 1 300
Circle -1 false false 0 0 300

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
Rectangle -1 false false 0 0 300 300

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
