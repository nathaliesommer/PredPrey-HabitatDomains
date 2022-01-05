;; DESCRIPTION:
; SA model for NULL sit and wait



extensions [
  table ]

globals [
  attack-hours ; hours to avoid today
  hour
  day
  mylist ; used to store information
  ranked-hours ; used to store information
  mapAware ; remembering the hours they are attacked (linked to the table extension)
]

breed [predators predator]
breed [preys prey]

patches-own [
  prey-patch? ; setting prey habitat domain
  predator-patch? ; setting predator habitat domain
  Checkerboard ; either a black or white patch
  Detection-prob ; how "detectable" prey are on a certain patch (black or white, values can be input)
  LOF ; an hourly, habitat-specific updated fear of patch.
  mapAware-black ; remembering where it was hunted and when (linked to the table extension)
  mapAware-white ; remembering where it was hunted and when (linked to the table extension)
  patchGroupB ; remembering what patch group prey were hunted in (linked to the table extension)
  patchGroupW; remembering what patch group prey were hunted in (linked to the table extension)
  patch-group ; new
 patch-interaction; patch-specific value

]


preys-own [
  fear-value ; counting the number of times a predator tried to kill prey
  nearby ; counts the number of incidents that could have occurred according to hunting type, but where prey went undetected
  p-white ; count the number of white patches walked on
  p-black ; count the number of black patches walked on
  domain-prey ; count number of patches prey stay in prey-only
  domain-overlap ; count the number of patches prey stay that poverlap with predators
  hourMap ; count the number of hours the prey were active
  dead? ; determines if prey has died or not
]


to setup
  clear-all
  reset-ticks

  ;ask patches with [pxcor mod 4 = 3 or pycor mod 4 = 3] [ set pcolor one-of remove 55 base-colors ]

  ask patches [

    if pxcor mod 2 = pycor mod 2 [set Checkerboard 1]
    if Checkerboard = 1 [ set pcolor white

      set Detection-prob Detection-prob-white
      set mapAware-white table:make ;;;  Setting memory table
      table:put mapAware-white 0 0
      table:put mapAware-white 1 0
      table:put mapAware-white 2 0
      table:put mapAware-white 3 0
      table:put mapAware-white 4 0
      table:put mapAware-white 5 0
      table:put mapAware-white 6 0
      table:put mapAware-white 7 0
      table:put mapAware-white 8 0
      table:put mapAware-white 9 0
      table:put mapAware-white 10 0
      table:put mapAware-white 11 0
      table:put mapAware-white 12 0
      table:put mapAware-white 13 0
      table:put mapAware-white 14 0
      table:put mapAware-white 15 0
      table:put mapAware-white 16 0
      table:put mapAware-white 17 0
      table:put mapAware-white 18 0
      table:put mapAware-white 19 0
      table:put mapAware-white 20 0
      table:put mapAware-white 21 0
      table:put mapAware-white 22 0
      table:put mapAware-white 23 0


    ]
    if Checkerboard = 0 [ set Detection-prob Detection-prob-black
      set mapAware-black table:make ;;;  Setting memory table for all black patches and all white patches (could just be mapAware)
      table:put mapAware-black 0 0
      table:put mapAware-black 1 0
      table:put mapAware-black 2 0
      table:put mapAware-black 3 0
      table:put mapAware-black 4 0
      table:put mapAware-black 5 0
      table:put mapAware-black 6 0
      table:put mapAware-black 7 0
      table:put mapAware-black 8 0
      table:put mapAware-black 9 0
      table:put mapAware-black 10 0
      table:put mapAware-black 11 0
      table:put mapAware-black 12 0
      table:put mapAware-black 13 0
      table:put mapAware-black 14 0
      table:put mapAware-black 15 0
      table:put mapAware-black 16 0
      table:put mapAware-black 17 0
      table:put mapAware-black 18 0
      table:put mapAware-black 19 0
      table:put mapAware-black 20 0
      table:put mapAware-black 21 0
      table:put mapAware-black 22 0
      table:put mapAware-black 23 0

    ]

  ]
  ;; Bottom row
    ask patches with [ pycor < 2 and pxcor < 2 ]  [set patch-group 1]
  ask patches with [ (pycor < 2) and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 2]
    ask patches with [ (pycor < 2) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 3]
    ask patches with [ (pycor < 2) and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 4]

  ;; Row 2
      ask patches with [ (pycor > 1) and (pycor < 4) and (pxcor < 2) ]  [ set patch-group 5]
  ask patches with [ (pycor > 1) and (pycor < 4)  and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 6]
    ask patches with [ (pycor > 1) and (pycor < 4) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 7]
    ask patches with [ (pycor > 1) and (pycor < 4)  and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 8]

  ;;;; row 3
  ask patches with [ (pycor > 3) and (pycor < 6) and (pxcor < 2) ]  [ set patch-group 9]
    ask patches with [ (pycor > 3) and (pycor < 6)  and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 10]
    ask patches with [ (pycor > 3) and (pycor < 6) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 11]
    ask patches with [ (pycor > 3) and (pycor < 6)  and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 12]


  ;;;Row 4
    ask patches with [ (pycor > 5) and (pycor < 8) and (pxcor < 2) ]  [ set patch-group 13]
    ask patches with [ (pycor > 5) and (pycor < 8)  and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 14]
    ask patches with [ (pycor > 5) and (pycor < 8) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 15]
    ask patches with [ (pycor > 5) and (pycor < 8)  and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 16]

    ;;;Row 5
    ask patches with [ (pycor > 7) and (pycor < 10) and (pxcor < 2) ]  [ set patch-group 17]
    ask patches with [ (pycor > 7) and (pycor < 10)  and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 18]
    ask patches with [ (pycor > 7) and (pycor < 10) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 19]
    ask patches with [ (pycor > 7) and (pycor < 10)  and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 20]


      ;;;Row 6
  ask patches with [ (pycor > 9) and (pycor < 12) and (pxcor < 2) ]  [ set patch-group 21]
  ask patches with [ (pycor > 9) and (pycor < 12)  and (pxcor > 1) and (pxcor < 4) ]  [set patch-group 22]
    ask patches with [ (pycor > 9) and (pycor < 12) and (pxcor > 3) and (pxcor < 6) ]  [ set patch-group 23]
    ask patches with [ (pycor > 9) and (pycor < 12)  and (pxcor > 5) and (pxcor < 8) ]  [ set patch-group 24]





  ;;;  Setting Size of Habitat Domain for Prey and Predator

  ask patches [ set prey-patch? false
  set predator-patch? false ]

  if Prey-Starting-Condition = "Small" [
    ask patches [
      ifelse pycor > 5  [set prey-patch? true ]
      [set pcolor grey]
    ]
  ]

  if Prey-Starting-Condition = "Large" [
    ask patches [
      set prey-patch? true]
  ]


  if Predator-Starting-Condition = "Small" [
    ask patches [
      if pycor > 5 [set predator-patch? true ]]
  ]

  if Predator-Starting-Condition = "Large" [
    ask patches [
      set predator-patch? True ]
  ]

  ask patches with [ prey-patch? = false ] [ set LOF 999]

  ;; Setting up Turtles
  create-predators 1
  create-preys 1


  ask predators [
    set shape "wolf"
    set size .5
    set color blue
  ;  set hunting-success 0
    move-to one-of patches with [predator-patch? = true ]
  ]

  ask preys [
    set shape "sheep"
    set size .5
    set color grey
    set p-white 0
    set p-black 0
    set hourMap table:make ;;;  Setting memory table
      table:put hourMap 0 0
      table:put hourMap 1 0
      table:put hourMap 2 0
      table:put hourMap 3 0
      table:put hourMap 4 0
      table:put hourMap 5 0
      table:put hourMap 6 0
      table:put hourMap 7 0
      table:put hourMap 8 0
      table:put hourMap 9 0
      table:put hourMap 10 0
      table:put hourMap 11 0
      table:put hourMap 12 0
      table:put hourMap 13 0
      table:put hourMap 14 0
      table:put hourMap 15 0
      table:put hourMap 16 0
      table:put hourMap 17 0
      table:put hourMap 18 0
      table:put hourMap 19 0
      table:put hourMap 20 0
      table:put hourMap 21 0
      table:put hourMap 22 0
      table:put hourMap 23 0
    set dead? FALSE
    let nearest-turtle min-one-of other turtles [distance myself]
    let habitat-domain patches with [distance nearest-turtle > 4] ; placing prey 4 patches away from a predator at initial conditions while still in habitat domain
    move-to one-of habitat-domain with [prey-patch? = true]

  ]

  ask patches [

    set mylist []
    set attack-hours []

  ]

  set mapAware table:make ;;;  Setting memory table
  table:put mapAware 0 0
  table:put mapAware 1 0
  table:put mapAware 2 0
  table:put mapAware 3 0
  table:put mapAware 4 0
  table:put mapAware 5 0
  table:put mapAware 6 0
  table:put mapAware 7 0
  table:put mapAware 8 0
  table:put mapAware 9 0
  table:put mapAware 10 0
  table:put mapAware 11 0
  table:put mapAware 12 0
  table:put mapAware 13 0
  table:put mapAware 14 0
  table:put mapAware 15 0
  table:put mapAware 16 0
  table:put mapAware 17 0
  table:put mapAware 18 0
  table:put mapAware 19 0
  table:put mapAware 20 0
  table:put mapAware 21 0
  table:put mapAware 22 0
  table:put mapAware 23 0
end


;;;  Main Model

to go
spatial-temporal-landscape
  ifelse [dead?] of one-of preys = TRUE [stop]
[ask preys [
     ;; adding to check if prey has died
    plan-next-hour
     if not hidden? [
      prey-move ]
  ] ]

  keeping-track-of-habitat
  keeping-track-of-time
  keeping-track-of-space

  ask predators [
    ifelse hour <= 11;; sleeping predators / predator not on the landscape ;; ONLY PART OF CODE CHANGED FOR NO PREDATORS
    [ show-turtle
      predators-move
   detect]
    [hide-turtle]
  ]

  time
  tick
  if [day] of one-of patches = 1825  [stop] ;; stopping after 365 days  ;; 1825 is 5 years

end

;;;  Submodels
to prey-move ;;; Need to think about new ways to move - move on patches based on fear value of patch, if there are a number of equal patch types, randomly chose one.
  ask preys [
  let p min-one-of neighbors in-radius 2 [LOF] ; i feel like they shouldn't be able to move 2 patches, especially if their whole habitat domain is 11x5 - to avoid being eaten by same pred over and over
      move-to p
  ]
end


to predators-move
  ask predators [

      if hour = 0
      [ move-to one-of patches with [predator-patch? = true]]
    ]

end


to detect
  ask predators [

    if any? preys-here [
      ask preys-here [set nearby nearby + 1]
      let detection [Detection-prob] of patch-here
      if random-float 1.0 <= detection [
        hunt]]  ]

end


to hunt
       ifelse  random 100 >= 99 [
    ask preys [set dead? TRUE ] ]
 [ask preys [set fear-value fear-value + 1
    set pcolor red]
        update-awareness-hour
        update-awareness-patch
        if [Checkerboard] of patch-here = 1
    [ask patches with [Checkerboard = 1] [update-awareness-white]
      let patch-name [patch-group] of patch-here
      ask patches with [(patch-group = patch-name) and (Checkerboard = 1)] [update-white-patch-group] ]

        if  [Checkerboard] of patch-here = 0
        [ask patches with [Checkerboard = 0] [update-awareness-black]
    let patch-name [patch-group] of patch-here
      ask patches with [(patch-group = patch-name) and (Checkerboard = 0)] [update-black-patch-group]
    ]
  ]
end


to update-awareness-white ;Setting up a memory table for white patches
  let time-now hour    ; key for table
  let attacks-white table:get-or-default mapAware-white time-now 0
  table:put mapAware-white time-now attacks-white + 1
end


to update-awareness-black ; Setting up a memory table for black patches
  let time-now hour    ; key for table
  let attacks-black table:get-or-default mapAware-black time-now 0
  table:put mapAware-black time-now attacks-black + 1
end

to update-white-patch-group
  set patchGroupW patchGroupW + 1
end

to update-black-patch-group
set patchGroupB patchGroupB + 1
end


to update-awareness-hour ; Setting up a memory table for all patches by the hour
  let time-now hour    ; key for table
  let attacks table:get-or-default mapAware time-now 0
  table:put mapAware time-now attacks + 1
end

to update-awareness-patch
    let patch-name [patch-group] of patch-here
    ask patches with [(patch-group = patch-name)][
      set patch-interaction patch-interaction + 1]

end

to spatial-temporal-landscape
; setting up the LOF per hour
  ask patches with [(Checkerboard = 1) and (prey-patch? = TRUE)][
    let time-next hour + 1
    ifelse table:has-key? mapAware-white time-next
    [set LOF table:get mapAware-white time-next]
    [set LOF 0]
     set LOF 0; set to 0
  ]

  ask patches with [(Checkerboard = 0) and (prey-patch? = TRUE)] [
    let time-next hour + 1
    ifelse table:has-key? mapAware-black time-next
    [ set LOF table:get mapAware-black time-next ]
    [ set LOF 0 ]

    set LOF 0 ; set to 0
  ]
;ask patches [
;    set LOF LOF + patch-interaction]  ;;;;;;;; HERE IS WHERE WE NEED TO ADD PATCH-SPECIFIC VALUES ;;;;;;;;;


; finding hours to avoid
  ask preys [
    if hour = 0 [
      let listA table:to-list mapAware
        let list-b [table:to-list mapAware-black] of one-of patches with [(Checkerboard = 0) and (prey-patch? = TRUE)]
      let list-w [table:to-list mapAware-white] of one-of patches with [(Checkerboard = 1) and (prey-patch? = TRUE)]
      let temp range length listA
      set mylist []
      foreach temp [
        i ->
        let white-attacks item i list-w
        let black-attacks item i list-b
        let attack-now-w item 1 white-attacks
        let attack-now-b item 1 black-attacks
        let the-hour item 0 black-attacks
        let lowest-v min (list attack-now-w attack-now-b) ; for each hour, selecting the lower value between the black and white patches
        set mylist lput (list the-hour 0) mylist ; Turn this to 0 for the null

      ]

      ask patch-here [
        set ranked-hours []
        set attack-hours []
        foreach mylist [ ; inserts random value
          [x] ->
          let random-value random-float 1.0
          let insert-randomv-list insert-item 1 (x) random-value
          set ranked-hours lput insert-randomv-list ranked-hours
        ]


        let sorted-list sort-by [[a b] -> (item 1 a) > (item 1 b)](ranked-hours) ; sort by the random values first to make it random
        let attacks-sorted-by sort-by [[a b] -> (item 2 a) > (item 2 b)](sorted-list) ; sort by the number of attacks

        set ranked-hours attacks-sorted-by

        let ChoosedList sublist ranked-hours 0 12 ; selecting the top 12 hours

        foreach ChoosedList [
          [x] ->
          let number-of-attacks item 2 (x)
          let hunted-hours item 0 (x)
          set attack-hours lput hunted-hours attack-hours
        ]
      ]
    ]
  ]
end


to plan-next-hour
  if not empty? attack-hours [
    foreach attack-hours [
      [x] -> let dangerous-hours (x)
      if hour = dangerous-hours
      [ set hidden? true
        stop]
      if hour !=  dangerous-hours
      [set hidden? false]
    ]
  ]
end

to keeping-track-of-habitat
    ask preys [
    if not hidden? [
  if [Checkerboard] of patch-here = 1 [
    set p-white p-white + 1]
  if [Checkerboard] of patch-here = 0 [
    set p-black p-black + 1 ]
  ]
  ]
end

to keeping-track-of-time
    ask preys [
  if not hidden? [
    let time-now hour    ; key for table
    let time-moving table:get-or-default hourMap time-now 0
    table:put hourMap time-now time-moving + 1 ]
    ]
end

to keeping-track-of-space
    ask preys [
    if not hidden? [
  if [predator-patch?] of patch-here = TRUE [
    set domain-overlap domain-overlap + 1]
  if [predator-patch?] of patch-here = FALSE [
    set domain-prey domain-prey + 1 ]
  ]
  ]
end


to time
  set hour hour + 1
  if hour = 24 [
    set day day + 1
    set hour 0]
end




;;;; THIS IS FOR REPORTING
to-report mapAware-table [ table_ ]
  let out []
  foreach table:to-list mapAware [ i ->
    set out lput ( reduce sentence i ) out
  ]
  report out
end

;mapAware-white

to-report white-table[ table_ ]
  let out []
  foreach table:to-list [mapAware-white ] of one-of patches with [(Checkerboard = 1) and (prey-patch? = TRUE)] [ i ->
    set out lput ( reduce sentence i ) out
  ]
  report out
end

to-report black-table[ table_ ]
  let out []
  foreach table:to-list [mapAware-black ] of one-of patches with [(Checkerboard = 0) and (prey-patch? = TRUE)] [ i ->
    set out lput ( reduce sentence i ) out
  ]
  report out
end

to-report hour-map-prey [ table_ ]
  let out []
  foreach table:to-list [hourmap] of one-of preys [ i ->
    set out lput ( reduce sentence i ) out
  ]
  report out
end
@#$#@#$#@
GRAPHICS-WINDOW
254
10
539
435
-1
-1
34.7
1
10
1
1
1
0
0
0
1
0
7
0
11
1
1
1
ticks
20.0

BUTTON
16
224
82
257
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
18
265
81
298
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
10
23
211
68
Predator-Starting-Condition
Predator-Starting-Condition
"Small" "Large"
1

CHOOSER
12
77
212
122
Prey-Starting-Condition
Prey-Starting-Condition
"Large" "Small"
0

INPUTBOX
98
225
184
285
hunting-starts
0.0
1
0
Number

SLIDER
12
305
210
338
Detection-prob-black
Detection-prob-black
0
1
0.9
.1
1
NIL
HORIZONTAL

SLIDER
10
347
213
380
Detection-prob-white
Detection-prob-white
0
1
0.2
.1
1
NIL
HORIZONTAL

MONITOR
817
77
1256
122
NIL
attack-hours
17
1
11

MONITOR
642
129
699
174
hours
hour
17
1
11

BUTTON
21
419
84
452
NIL
NIL
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

This model seeks to explore how caribou move nutrients around in the summer range

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="YEAR_MODELS" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="8760"/>
    <metric>ticks</metric>
    <metric>[nearby] of preys</metric>
    <metric>mapAware-table [1] of one-of patches</metric>
    <metric>hour-map-prey [1]</metric>
    <metric>[p-black] of one-of preys</metric>
    <metric>[p-white] of one-of preys</metric>
    <metric>[domain-overlap] of one-of preys</metric>
    <metric>[domain-prey] of one-of preys</metric>
    <enumeratedValueSet variable="Predator-Strategy">
      <value value="&quot;Sit-and-Wait&quot;"/>
      <value value="&quot;Sit-and-Pursue&quot;"/>
      <value value="&quot;Active&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prey-Starting-Condition">
      <value value="&quot;Small&quot;"/>
      <value value="&quot;Large&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Predator-Starting-Condition">
      <value value="&quot;Small&quot;"/>
      <value value="&quot;Large&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Detection-prob-black">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Detection-prob-white">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="YEAR5_MODELS" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="43800"/>
    <metric>ticks</metric>
    <metric>[nearby] of preys</metric>
    <metric>mapAware-table [1] of one-of patches</metric>
    <metric>hour-map-prey [1]</metric>
    <metric>[p-black] of one-of preys</metric>
    <metric>[p-white] of one-of preys</metric>
    <metric>[domain-overlap] of one-of preys</metric>
    <metric>[domain-prey] of one-of preys</metric>
    <enumeratedValueSet variable="Predator-Strategy">
      <value value="&quot;Sit-and-Wait&quot;"/>
      <value value="&quot;Sit-and-Pursue&quot;"/>
      <value value="&quot;Active&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Prey-Starting-Condition">
      <value value="&quot;Small&quot;"/>
      <value value="&quot;Large&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Predator-Starting-Condition">
      <value value="&quot;Small&quot;"/>
      <value value="&quot;Large&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Detection-prob-black">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Detection-prob-white">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
