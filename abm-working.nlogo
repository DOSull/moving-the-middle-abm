__includes [
  "mtm-read-files.nls"
  "mtm-geography.nls"
  "mtm-utils.nls"
  "mtm-profile.nls"
  "mtm-render.nls"
  "mtm-farmer.nls"
  "mtm-farm.nls"
  "mtm-holding.nls"
  "distribution-utils.nls"
  "list-utils.nls"
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
  ;; sr
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
  ;; TO ADD
  farm-type-suitabilities ;; table of list of suitabilities by LUC for each farm-type
  farm-type-change-probs  ;; table of costs to convert between farm types
  mgmt-intervention-types ;; list of named management interventions - more likely to change over time
  mgmt-interventions      ;; table of tables of management intervention impacts
  dispositions            ;; list of farmer dispositions

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
  m-suitabilities
  m-change-costs
  ;; a column matrix ordered by farm-types list
  m-prices
  ;; rows-cols entries for intervention-type - farm-type respectively
  ;; ordered by intervention-types and farm-types lists
  m-mgmt-intervention-cost-impacts
  m-mgmt-intervention-yield-impacts
  m-mgmt-intervention-emissions-impacts

  ;; colour settings - these can be changed in one place, see setup-key-colours procedure
  colour-key              ;; table of colour settings
]

undirected-link-breed [industry-links industry-link]
undirected-link-breed [local-links local-link]
breed [farmers farmer]    ;; one farmer per farm
breed [farms farm]        ;; turtle where most of the action happens
breed [holdings holding]
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
  set dispositions [ "for-profit" "pro-social" "pro-environmental" ]

  setup-colours ;; must come after reading farmer parameters since it depends on farm types
  ask patches [ set pcolor table:get colour-key "background" ]

  setup-geography
  setup-economic-parameters
  make-matrix-copies-of-data

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
  ifelse not force? and stop-model? [
    cleanup
    stop
  ]
  [ ;; update farm status - note that succession may lead to change of farm type
    ask farmers [ age-and-succeed-farmer ]
    ask holdings [ update-profit-of-holding ]
    ask farms [ update-profit-of-farm ]
    ask farmers [
      let loss-making-holdings get-loss-making-holdings
      ifelse any? loss-making-holdings [
        ;; we're making some losses, so consider wholesale change and/or piecemeal change
        ;; probability of wholesale change is down-weighted by how much of the land is losing money
        let down-weight sum [count the-land] of loss-making-holdings / count the-land
        ;; collect the whole farm change options
        let options consider-farm-type-change [] down-weight "Losing money: " false
        ;; add in the holdings change to forestry options
        set options consider-forestry options "Some holdings losing money: " false
        ;; evaluate and act
        make-farm-type-changes options
      ]
      [ ;; we're doing OK so think about management interventions instead
        if any? my-holdings with [any-interventions-to-do?] [
          ;; this reports a [holding [intervention-type probability]] list
          let potential-changes consider-management-changes "Management change: " false
          ;; which we pass to make-management-changes
          make-management-changes potential-changes
        ]
      ]
    ]
    ask farms [redraw-farm]
    ask holdings [redraw-holding]
    tick
  ]
end


;; any model stop condition goes here
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


;; -----------------------------------------
;; reset code
;; these procedures store the initial state
;; of the model to allow quick reset
;; -----------------------------------------

to store-initial-values
  ask farm-land [ set landuse-0 landuse ]
  ask holdings [
    set farm-type-0 farm-type
    set current-profit-0 current-profit
    set current-income-0 current-income
    set current-costs-0 current-costs
    ;; important to COPY here to get a new matrix, not a reference to the old one
    set my-mgmt-interventions-0 matrix:copy my-mgmt-interventions
    set avail-mgmt-interventions-0 matrix:copy avail-mgmt-interventions
    set landuse-luc-profile-0 matrix:copy landuse-luc-profile
  ]
  ask farms [
    set farm-type-0 farm-type
    set current-profit-0 current-profit
    set current-income-0 current-income
    set current-costs-0 current-costs
  ]
  ask farmers [
    set farm-type-0 farm-type
    set disposition-0 disposition
    set age-0 age
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
    set my-mgmt-interventions matrix:copy my-mgmt-interventions-0
    set avail-mgmt-interventions matrix:copy avail-mgmt-interventions-0
    set landuse-luc-profile matrix:copy landuse-luc-profile-0
  ]
  ask farms [
    set farm-type farm-type-0
    set current-profit current-profit-0
    set current-income current-income-0
    set current-costs current-costs-0
  ]
  ask farmers [
    set farm-type farm-type-0
    set disposition disposition-0
    set age age-0
  ]
  redraw-farms-and-holdings
  reset-ticks
  tick
  if run-rng-seed != 0 [ random-seed run-rng-seed ]
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
832
921
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
1
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
1
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
90.0
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
31
524
195
557
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
123
597
197
630
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
48
639
197
672
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
48
680
197
713
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
32
326
196
359
show-landuse?
show-landuse?
0
1
-1000

SWITCH
31
461
195
494
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
32
561
200
590
More intense colours are lower LUC values (better land).
11
0.0
1

TEXTBOX
35
363
187
447
Colour key (applies to both landuse and farm symbols)\n  SNB - Brown\n  Dairy - Blue\n  Forestry - Green \n  Crop -Purple
11
0.0
1

TEXTBOX
32
498
200
516
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
2.0
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
0.0
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
48
721
198
754
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
33
258
199
291
show-events?
show-events?
1
1
-1000

TEXTBOX
40
294
190
322
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

SWITCH
99
97
202
130
force?
force?
1
1
-1000

TEXTBOX
16
96
97
169
Use this to force model steps even if stop condition is met
11
0.0
1

BUTTON
48
761
197
794
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
28
805
196
838
show-local-links?
show-local-links?
1
1
-1000

SWITCH
28
842
196
875
show-industry-links?
show-industry-links?
1
1
-1000

SWITCH
27
878
196
911
include-networks?
include-networks?
1
1
-1000

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
