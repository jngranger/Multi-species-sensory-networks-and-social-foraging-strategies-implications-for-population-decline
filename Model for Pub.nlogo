;Initialize globals, patch and turtle variables

;extensions [palette] ;only necessary if using transparency
breed [wa was]        ;These are the arrows showing the wind direction (WA=Wind Arrow)
breed [odor odors]    ;These are agents that only exist during the set up and create the odor plumes
breed [bird birds]    ;The agents that search for food

bird-own
[
  detect-dist-food                   ;distance from which birds can directly detect food patches
  max-align-cohere-turn              ;maximum turning angle for aligning and cohering
  max-separate-turn                  ;maximum turning angle for separating
  food-nearby                        ;a vector to contain all food within sight-range
  flockmates                         ;a vector to contain all flockmates within sight-range
  smelling-mates                     ;a vector to contain all flockmates who are signalling that they are smelling the food within sight-range
  nearest-neighbor                   ;the closest flockmate
  average-heading-mates              ;the average heading of all mates
  average-heading-towards-mates      ;the heading that would take you towards the centroid of all mates
  did-find-food                      ;did you find the food? yes (1) or no (0)
  ever-find-odor                     ;did you ever, at any point in time, encounter an odor? yes (1) or no (0)
  state                              ;lets agents switch between left and right for zig zag headings
  time-spent-zigzag                  ;time spent maintaining heading for each heading within the zigzag
  timer-for-zigzag-switch            ;a timer to have agents keep their headings for at least 5 steps for each of the "crosswind casts" (referred to as zig-zag in code)
  time-spent-upwind                  ;time spent moving upwind after losing an odor
  time-spent-olfsearch               ;total time spent performing the olfactory search algorithm (essentially, the total time spent performing the zig zag search, plus time-spent upwind)
  timer-since-odor                   ;time since you last encountered an odor
  odor-intensity                     ;same as olfaction-threshold unless it's a mixed group
  dd                                 ;same as detection-distance unless it's a mixed group
  minimum-separation
]

patches-own
[
  wind      ;wind direction
  food      ;are you a food patch, yes (1) or no (0)
  intensity ;intensity of odor--owned by the patch
]

globals
[y-start] ; the y-coord of the center of the starting location square for the birds

to setup
  ca ;clear all
  random-seed Seed ;set seed
  set-up-world     ;create the wind and food patch
  make-odor-plumes ;makes the odor plume

  ;set the y-coord of the center of the starting location square for the birds
  set y-start min [pycor] of patches with [intensity > 0 ]

  ;initialize the birds
  make-birds

  reset-ticks ;reset ticks = 0
end

to set-up-world
  ask patches
  [
    set wind random 359 ;Random wind direction between 0 and 359
    set food 0          ;all patches start as NOT food
    set pcolor white    ;all patches start out white
    set intensity 0     ;all patches start with an odor intensity of 0
  ]

  ;This has each patch set their wind direction to be the linear mean of the wind direction for all neighbors, resulting in an average wind direction of ~mod(180) with patchy noise
  ask patches [set wind mean [wind] of neighbors]

  ;Visualize the wind using the see-wind-arrows button in the GUI and uncommenting the line below (default commented out to save time during runs)
  ;if see-wind-arrows = True [ make-wind-arrows]

  ;make food patch
  ;;food patch's center x-coord is 3/4ths of the way to the right (x = 225) and the center y-coord is at 1/5 of the way from the top (y = 240)
  ;;radius = 9 patches
  ask patch ((3 * max-pxcor) / 4 )  (max-pycor - (max-pycor / 5))
    [
      ask patches in-radius 9
      [ set food 1
        set pcolor black ]
  ]
end

to make-wind-arrows
  ;This algorithm cretes the arrows pointing in the wind direction
  ;;One arrow per every 10x10 patch block
  ;;Recommended to uncomment the palette extension at the beginning of code and the transparency line below (default commented out to save time during runs as palette extension can take awhile to load)
  ask patches
  [if pxcor mod 10 = 0 and pycor mod 10 = 0
    [
     sprout-wa 1
      [
      set heading [wind] of patch-here
      set color blue
      set size 6
      set shape "arrow"
      ;palette:set-transparency 70
      ]
    ]
  ]
end

to make-odor-plumes

  ;each food patch sprout an odor. The odors are green
  ask patches
  [
    if food = 1
    [sprout-odor 1 [ set color green ] ]
  ]

  ;Odors then travel along the direction of the wind for a number of steps set by the UI slider Length-Odor-Trails (default = 200)
  repeat Length-Odor-Trails
  [
    ask odor
    [
      ;the odor moves in the direction of the wind at their current patch
      set heading wind
      forward 1
      ;the patches record the total number of odors that pass through them, thus the patch's intensity = the total number of odorants that ever passed through it
      ask patch-here [set intensity intensity + 1]
    ]

  ]

  ask odor [die] ;once the loop finishes, the odors go away

  ;patches set their color by their intensity
  ask patches
  [ if pcolor != black                               ;only update the color if it's not already black
    [set pcolor scale-color green intensity 150 1]   ;set color green scaling with intensity (darker color = higher intensity).
  ]
end

to make-birds

  create-bird (N-Birds) ;create N birds (set by GUI). Generally N=50
  [
    ;Set the starting location to be randomly located within a 20x20 area, where the center x-coord= 100, and center y-coord is defined in setup to be the bottom of the plume (shown in GUI monitor)
    setxy ((random-float 20 - 10) + 100) ((random-float 20 - 10) + y-start)
    set heading [wind] of patch-here - 90              ;move crosswind to start
    set size 5                                         ;set size to 5 (does not impact results, this just makes them easier to see)
    set color black                                    ;default color is black
    set detect-dist-food 5                             ;agents can directly detect food from 5 patches away
    set max-align-cohere-turn 40                       ;max turning angle for aligning and cohering is 40 degrees
    set max-separate-turn 2                            ;max turning angle for separating is 2 degrees
    set food-nearby nobody                             ;no food nearby
    set flockmates no-turtles                          ;no flockmates
    set smelling-mates no-turtles                      ;no birds smelling nearby
    set nearest-neighbor no-turtles                    ;no nearest neighbor
    set did-find-food 0                                ;start without having found food (yes=1, no=0)
    set ever-find-odor 0                               ;start without ever having smelt an odor (yes=1, no=0)
    set state random 2                                 ;randomly either 1 or 0 -- direction of first zig zag can be either left or right
    set time-spent-zigzag 5                            ;total time spent maintaining heading during each leg of zigzag is 5 ticks
    set timer-for-zigzag-switch time-spent-zigzag + 1  ;this way the zig zag iterates upon first running
    set time-spent-upwind 15                           ;total time spent moving upwind after initially losing the odor
    set time-spent-olfsearch 75                        ;total time spent performing the olfactory search is 75 ticks (15 ticks upwind, 60 ticks zigzaging)
    set timer-since-odor time-spent-olfsearch + 1      ;did not recently encounter the odor plume
    set odor-intensity Olfaction-Threshold
    set dd Detection-Distance
  ]

  ask bird [if pen = TRUE [pen-down]]

  if Mixed-Group = TRUE
  [
    ask bird [
      set odor-intensity 1
      set dd 10

    ]
    ask n-of  Number-Of-Low-Olf bird [
      set odor-intensity 4000
      set dd 50
      set color grey
    ]
    ]

    ;If separation and vision are interdependent (true except for parameterization), then set the separation distance (which is otherwise set in the GUI) to be 1/10th the detection distance
  ask bird [ set minimum-separation ((dd / 10) + 1)]


end

to go
  tick ;increase tick by 1

  ask bird [update-odor-timer ]  ;This updates the odor timer (or timer-since-odor). It must happen outside of the movement algorithm

  ask bird [Movement-Algorithm]  ;Main movement algorithm

  ask bird [forward 1 ]          ;All agents move forward one patch once the movement algorithm is complete

  if ticks >= 600 or (sum [did-find-food] of bird) = N-Birds [stop]
end

to update-odor-timer ;This must iterate every tick BEFORE the movement algorithm is run
  ;Are you currently in an odor plume?
  ifelse ([intensity] of patch-here) > odor-intensity
    [ set timer-since-odor 0                        ;if yes, set timer-since-odor 0 and mark that you found the odor plume at least once
      set ever-find-odor 1
    ]
    [ set timer-since-odor (timer-since-odor + 1) ] ;if no, add one to timer-since-odor

  ;timer for maintaining heading for the zigzags increases by 1 every tick. This ensures it will always be > time-spent-zigzag upon first entering the zigzag algorithm
  set timer-for-zigzag-switch (timer-for-zigzag-switch + 1)

end

to Movement-Algorithm

  ;Main Movement Algorithm. An if-else chain to prioritize movement
  ;;Order: Found food->See food->Smell food->Follow->ZigZag->Flock->Crosswind
  ifelse [food] of patch-here = 1 or did-find-food = 1    ;IF you found the food....
  [found-food]                                              ;...YES: stay on the food patch (and change color)
  [search-food                                              ;...NO: look for food within visual range. Outputs a list of patches within detect-dist-food that are food (0 if none)

    ifelse count food-nearby > 0                          ;IF you SEE FOOD...
    [face min-one-of food-nearby [distance myself ] ]       ;...YES: Move towards the nearest food you can see
    [                                                       ;...NO: check to see if you are in the plume

      ifelse timer-since-odor <= time-spent-upwind        ;IF you are IN the plume... (timer <= 15)
      [in-odor-plume ]                                      ;...YES: move upwind (also changes color)
      [
        if communicate = TRUE                               ;...NO: check to see if anyone else nearby is smelling (only runs if communicate=TRUE, otherwise always none)
        [find-smelling]                                       ;;Outputs agents AHEAD of you, within range, that are signalling, i.e. are orange (0 if none)
        set color black                                       ;;reset color for agents who are no longer signalling

        ifelse any? smelling-mates                        ;IF any agents nearby are signalling...
          [follow-others]                                   ;...YES: move towards any signalling agents (also resets timers and changes color)
          [                                                 ;...NO: check to see if you *recently* found the plume but lost it

          ifelse timer-since-odor <= time-spent-olfsearch ;IF you encountered the plume but recently lost it (15 < timer < 76)...
          [set heading zig-zag-heading]                     ;...YES: ZigZag
          [
            if weight-flock-crosswind > 0                   ;...NO: look for flockmates. (only runs if weight-flock-crosswind>0, otherwise always none)
            [find-flockmates]                                 ;;Outputs other agents within range (0 if none)

            ifelse any? flockmates                        ;IF you have any nearby flockmates...
            [flock-crosswind]                               ;...YES: output flocking algorithm heading. If weight-flock-crosswind = 0 this is equivalent to moving crosswind
            [set heading ([wind] of patch-here - 90 )]      ;...NO: move crosswind (to the right)
          ]
        ]
      ]
    ]
  ]

end

to found-food

  if did-find-food = 0 ;only run once, the first time you encounter the food patch
  [
    set did-find-food 1           ;mark that you found the food
    set color green               ;color change
  ]

  ;This ensures they stay on the food patch
  ifelse pcolor = black                                    ;IF you're standing on the food patch...
  [set heading heading + random-normal 0 90]               ;...YES: move randomly
  [face one-of patches in-radius 2 with [pcolor = black] ] ;...NO: set your heading to move back to the food patch

end

to search-food
  set food-nearby patches in-radius detect-dist-food with [food = 1]   ;Locate any food patches within a 5 patch radius that are food
end

to in-odor-plume
  set heading [wind] of patch-here - 180               ; move upwind
  set color orange                                     ;color change to indicate agents that are actively smelling the plume (signal)
end

to find-smelling
  set smelling-mates other bird in-cone dd 180 with [color = orange] ;Find all birds within range AHEAD of you (no backtracking) that are smelling (orange)
end

to follow-others
  set color magenta   ;color change. Doesn't do anything just helps the viewer keep track of what is occurring

  ;align and cohere with signalling birds. Because this isn't weighted by crosswind movement, results in direct movement towards signalling birds within range
  align smelling-mates
  cohere smelling-mates

  set timer-since-odor (time-spent-upwind + 1)        ;reset timer-since-odor to 6 (results in agents who found-a-signaler-but-then-they-stopped-signalling behave as if they found an odor but lost it, i.e. zigzag)
  set timer-for-zigzag-switch (time-spent-zigzag + 1) ;reset timer for maintaining zigzag heading so that it is >time-spent-zigzag upon first entering the zigzag algorithm
end

to-report zig-zag-heading

  ;We want to zig-zag every 5 steps. Use time-since-odor to maintain heading for 5 steps before switching direction

  ;If you've been moving in the same direction for more than 5 steps...
  if timer-for-zigzag-switch >= time-spent-zigzag
  [
    set timer-for-zigzag-switch 0                 ;reset timer
    set state state + 1                           ;change heading to the opposite direction of last time
  ]

  ;Set your heading to be either slightly to the left or slightly to the right of upwind
  ifelse state mod 2 = 0
  [ report ((wind - 180)  +  45) ] ;Set heading upwind + an offset (45 degrees to the right)
  [ report ((wind - 180)  -  45) ] ;Set heading upwind - an offset (45 degrees to the left)

end

to find-flockmates
   set flockmates other bird in-radius dd ;Find all birds within range
end

to flock-crosswind

  set nearest-neighbor min-one-of flockmates [distance myself] ;find nearest neighbor

  ;If you are too close to your nearest neighbor, separate, otherwise, align and cohere
  ;;After iterating, heading = flocking heading
  ifelse distance nearest-neighbor < minimum-separation
  [ separate ]
  [ align flockmates
    cohere flockmates
  ]

  let crosswind ([wind] of patch-here - 90) ;find the crosswind heading

  ;Take the weighted mean of the flocking heading and the crosswind heading
  ;;because headings are circular, the circular average must be taken of the two headings using trig
  ;;if weight-flock-crosswind = 0 this is equivalent to setting the heading to crosswind
  ;;if weight-flock-crosswind = 1 this is equivalent to setting the heading to heading
  let mean-x  (( weight-flock-crosswind * sin heading) + ((1 - weight-flock-crosswind) * sin crosswind))
  let mean-y  (( weight-flock-crosswind * cos heading) + ((1 - weight-flock-crosswind) * cos crosswind))

  ;this prevents errors that occur when taking the atan of 0,0
  ifelse mean-x = 0 and mean-y = 0
    [ set heading heading  ]
    [ set heading atan mean-x mean-y ]
end

to separate
 ;this prevents the problem where if the nearest neighbor and you have the same heading you end up not changing your heading (which almost never occurs)
  ifelse heading = ([heading] of nearest-neighbor)
   [turn-away (towards nearest-neighbor + 180) ] ;set heading to be away from nearest neighbor (towards + 180 = away)
   [turn-away ([heading] of nearest-neighbor) ]  ;set heading to turn away from nearest neighbor
end

to align [mates]
  ;Get the circular average of all flockmate's headings
  let x-component sum [dx] of mates
  let y-component sum [dy] of mates
  ifelse x-component = 0 and y-component = 0
    [ set average-heading-mates heading ]
    [ set average-heading-mates atan x-component y-component ]

  turn-towards average-heading-mates
end

to cohere [mates]
  ;Get the circular mean heading of the direction that would take the agent towards all flockmates
  ;; "towards myself" gives us the heading from the other turtle to me, but we want the heading from me to the other turtle, so we add 180
  let x-component mean [sin (towards myself + 180)] of mates
  let y-component mean [cos (towards myself + 180)] of mates
  ifelse x-component = 0 and y-component = 0
    [ set average-heading-towards-mates heading ]
    [ set average-heading-towards-mates atan x-component y-component ]

  turn-towards average-heading-towards-mates
end

to turn-towards [new-heading]
  turn-at-most (subtract-headings new-heading heading) max-align-cohere-turn
end

to turn-away [new-heading]
  turn-at-most (subtract-headings heading new-heading) max-separate-turn
end

to turn-at-most [turn max-turn]
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end



to export-wind-to-csv
  ; Open a file for writing
  file-open "C:/Users/Jesse/Duke Bio_Ea Dropbox/Jesse Granger/Jesse/Duke/Research/Bird Foraging_Gabby Nevitt/Starting to think about publication/Models and Code For Publication/WindField.csv"

  ; Write the header row
 ; file-write "pxcor,pycor,wind"
  file-print "pxcor,pycor,wind"

  ; Loop over all patches and write their data
  ask patches [
    let px pxcor
    let py pycor
    let patch-var wind  ; replace 'patch-variable' with the actual name of your patch variable
    ;file-write
    file-print (word px "," py "," patch-var)
  ]

  ; Close the file
  file-close
end


to export-world-to-csv
  ; Open a file for writing
  file-open "C:/Users/Jesse/Duke Bio_Ea Dropbox/Jesse Granger/Jesse/Duke/Research/Bird Foraging_Gabby Nevitt/Starting to think about publication/Models and Code For Publication/World Data.csv"

  ; Write the header row
 ; file-write "pxcor,pycor,wind"
  file-print "pxcor,pycor,wind,intensity,food"

  ; Loop over all patches and write their data
  ask patches [
    let px pxcor
    let py pycor
    let patch-var1 wind
    let patch-var2 intensity
    let patch-var3 food
    ;file-write
    file-print (word px "," py "," patch-var1 "," patch-var2 "," patch-var3)
  ]

  ; Close the file
  file-close
end

to export-bird-to-csv
  ; Open a file for writing
  file-open "C:/Users/Jesse/Duke Bio_Ea Dropbox/Jesse Granger/Jesse/Duke/Research/Bird Foraging_Gabby Nevitt/Starting to think about publication/Models and Code For Publication/Bird Location.csv"

  ; Write the header row
 ; file-write "pxcor,pycor,wind"
  file-print "xcor,ycor"

  ; Loop over all patches and write their data
  ask bird [
    let x xcor
    let y ycor
    ;file-write
    file-print (word x "," y )
  ]

  ; Close the file
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
517
21
1127
632
-1
-1
2.0
1
10
1
1
1
0
1
1
1
0
300
0
300
0
0
1
ticks
30.0

BUTTON
91
43
154
76
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
166
43
229
76
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

SLIDER
279
83
458
116
Olfaction-Threshold
Olfaction-Threshold
1
40
40.0
1
1
NIL
HORIZONTAL

SLIDER
1139
24
1311
57
Length-Odor-Trails
Length-Odor-Trails
0
200
200.0
1
1
NIL
HORIZONTAL

SLIDER
291
376
463
409
N-Birds
N-Birds
0
50
50.0
1
1
NIL
HORIZONTAL

SLIDER
93
83
266
116
weight-flock-crosswind
weight-flock-crosswind
0
1
0.95
0.05
1
NIL
HORIZONTAL

SLIDER
280
121
452
154
Detection-Distance
Detection-Distance
0
100
50.0
10
1
NIL
HORIZONTAL

SWITCH
331
326
429
359
pen
pen
1
1
-1000

PLOT
92
168
453
318
% Found Food
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (((sum [did-find-food] of bird) / N-Birds)) * 100"

SWITCH
1143
65
1291
98
see-wind-arrows
see-wind-arrows
0
1
-1000

SWITCH
94
121
266
154
communicate
communicate
1
1
-1000

MONITOR
93
325
150
370
NIL
y-start
17
1
11

MONITOR
160
325
318
370
NIL
max [intensity] of patches
17
1
11

SLIDER
291
418
463
451
Seed
Seed
0
500
496.0
1
1
NIL
HORIZONTAL

SWITCH
91
417
217
450
Mixed-Group
Mixed-Group
0
1
-1000

SLIDER
93
377
275
410
Number-Of-Low-Olf
Number-Of-Low-Olf
0
N-Birds
25.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
to odor-plumes-x
    ask patches
  [
   if food = 1
   [
      sprout-odor 1
      [
        set color green
        set intensity 0
      ]
    ]
  ]

  ask odor
  [
    set heading wind
    forward 1
    set intensity intensity + 1
    if intensity > 99
    [die]
   ; set color scale-color red intensity 25 0
    palette:set-transparency intensity

  ]

end

;to odor-plumes

  
  
  ;  ask patches
;  [
;    if food = 1
;    [
;      ask patch-at-heading-and-distance (wind) 1
;      [
;        set intensity 50
;        set pcolor scale-color green intensity 100 0
;      ]
;    ]
;  ]

;  ask patches
;  [
 ;   let target-patch min-one-of (patches in-radius 300 with [food = 1]) [distance myself]
 ;   set intensity 100 - distance target-patch
 ;   set pcolor scale-color green intensity 100 0
;  ]
;end



ifelse ([intensity] of patch-here) > Olfaction-Threshold
  [
    set time-since-last-found 0
    set was-in-odor-plume in-odor-plume
    set in-odor-plume 1
  ]
  [
    set time-since-last-found time-since-last-found + 1
    set was-in-odor-plume in-odor-plume
    set in-odor-plume 0
  ]
    
  ifelse time-since-last-found < 20
  [
    ifelse was-in-odor-plume = 0 and in-odor-plume = 1
    [
      set heading [wind] of patch-here - 180 + random-normal 0 45
    ]
    [
      ifelse was-in-odor-plume = 1 and in-odor-plume = 1
      []
      [
        ifelse was-in-odor-plume = 1 and in-odor-plume = 0
        [
          set heading [wind] of patch-here - 180 + random-normal 0 45
        ]
        [ ;0 and 0
          set heading [wind] of patch-here - 180 + random-normal 0 45
        ]
      ]
      
    ]
    forward 1
  ]



; ifelse time-since-odor > 5 ;timer to keep heading for 5 steps
;  [
;    set time-since-odor 0 ;reset timer and change heading
;    ;set heading opposit of last time
;    ifelse state = 1
;    [
;      set state 0
;      set heading wind - 180  +  abs (Length-Odor-Trails - intensity )
;    ]
;    [
;      set state 1
;      set heading wind - 180  -  abs (Length-Odor-Trails - intensity )
;    ]
;    
;  ]
;  [
;    set time-since-odor time-since-odor + 1
;  ]
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="WindField" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go
export-wind-to-csv</go>
    <timeLimit steps="1"/>
    <enumeratedValueSet variable="seed">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Fig 3A" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 0 and did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1 and did-find-food = 0])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Birds">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Length-Odor-Trails">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-separation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sep-Vision-Interdependent">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Detection-Distance" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="communicate">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flock-crosswind">
      <value value="0"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Olfaction-Threshold">
      <value value="1"/>
      <value value="40"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="World" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>export-world-to-csv</go>
    <timeLimit steps="1"/>
    <enumeratedValueSet variable="seed">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Birds" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>export-bird-to-csv</go>
    <timeLimit steps="1"/>
    <enumeratedValueSet variable="seed">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Fig 3B Pop 10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 0 and did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1 and did-find-food = 0])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Birds">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Length-Odor-Trails">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-separation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sep-Vision-Interdependent">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Detection-Distance" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="communicate">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flock-crosswind">
      <value value="0"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Olfaction-Threshold">
      <value value="1"/>
      <value value="40"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 3B Pop 20 30 40" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 0 and did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1 and did-find-food = 0])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Length-Odor-Trails">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-separation">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sep-Vision-Interdependent">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Detection-Distance" first="10" step="10" last="50"/>
    <enumeratedValueSet variable="communicate">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flock-crosswind">
      <value value="0"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Olfaction-Threshold">
      <value value="1"/>
      <value value="40"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
    <enumeratedValueSet variable="N-Birds">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Fig 4_NumNoOlf_" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 0 and did-find-food = 1])</metric>
    <metric>(count turtles with [ever-find-odor = 1 and did-find-food = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and dd = 50])</metric>
    <metric>(count turtles with [did-find-food = 1 and dd = 10])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Birds">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Length-Odor-Trails">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Olfaction-Threshold">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Detection-Distance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="communicate">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flock-crosswind">
      <value value="0"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
    <enumeratedValueSet variable="Mixed-Group">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-Of-Low-Olf">
      <value value="50"/>
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
