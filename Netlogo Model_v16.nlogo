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
  max-turning-radius                 ;maximum turning radius (used for turning upwind and crosswind casts)
  food-nearby                        ;a vector to contain all food within sight-range
  flockmates                         ;a vector to contain all flockmates within sight-range
  smelling-mates                     ;a vector to contain all flockmates who are signalling that they are smelling the food within sight-range
  nearest-neighbor                   ;the closest flockmate
  average-heading-mates              ;the average heading of all mates
  average-heading-towards-mates      ;the heading that would take you towards the centroid of all mates
  did-find-food                      ;did you find the food? yes (1) or no (0)
  ever-find-odor                     ;did you ever, at any point in time, encounter an odor? yes (1) or no (0)
  state                              ;lets agents switch between going left vs right for zig zag headings
  time-between-turns                 ;time spent maintaining heading for each turn during the zigzag, and the time spent continuing to move upwind when the odor is initially lost (~15s)
  time-spent-olfsearch               ;total time spent performing the olfactory search algorithm (essentially, the total time spent performing the zig zag search, plus time-spent upwind)
  timer-for-zigzag-switch            ;a timer to have agents maintain their headings for a set amount of time for each turn in the "crosswind casts" (referred to as zig-zag in code)
  timer-since-odor                   ;time since you last encountered an odor
  odor-intensity                     ;olfaction-threshold, set per species for a mixed group
  dd                                 ;detection-distance, set per species for a mixed group
  flightspeed                        ;flight-speed, set per species for a mixed group
  flightheight                       ;not currently used for anything but could be used to set DD
  minimum-separation                 ;minimum-distance between you and other birds, set by DD per species for a mixed group
  species
]

patches-own
[
  wind      ;wind direction
  food      ;are you a food patch, yes (1) or no (0)
  intensity ;intensity of odor --> as owned by the patch
]

globals
[
  Patch-Scaler ;spatial resolution
  Tick-Scaler ;temporal resolution
  weight-flock-crosswind ;how much the birds prioritize moving towards other birds vs moving crosswind. 0 = Only crosswind, 1 = Only flocking
  ;location of the food patch
  Food-x
  Food-y
  ;Thresholds for Birds with High vs. Low vs. No Olfcation. Set based on the odor plume
  Odor-Threshold-High
  Odor-Threshold-Low
  Odor-Threshold-None
  N-Birds ;total number of birds (N-Species 1 + N-Species 2)
  allfoundodor ; the total number of birds who ever found the odor (necessary to set as a global b/c birds despawn)
  Flocking ;True if Flock Foraging or Network Foraging
  Cuing ;True if Local Enhancement or Network Foraging
  maxintensity
]

odor-own
[
  r_t ;Size at time = t
  r_o ;Initial size
  c_o ;Initial concentration
  c_t ;Concentration at time = t
  t
]

to setup
  ca ;clear all
  reset-ticks
  random-seed Seed ;set seed

  set Patch-Scaler 6
  set Tick-Scaler 0.6
  set N-Birds (N-Species1 + N-Species2 ) ;total number of birds

  set allfoundodor 0 ;no odor found at start

  ifelse Strategy = "Olfaction Only"
  [
    set Flocking FALSE
    set Cuing FALSE
  ]
  [ifelse Strategy = "Flock Foraging"
    [
      set Flocking TRUE
      set Cuing FALSE
    ]
    [ifelse Strategy = "Local Enhancement"
      [
        set Flocking FALSE
        set Cuing TRUE
      ]
      [
        set Flocking TRUE
        set Cuing TRUE
      ]

    ]

  ]

  ;Set the weighting for the flocking algorithm
  ;;If Flocking is true, set the weight based on the GUI value, if false, set to 0
  ifelse Flocking = TRUE
  [set weight-flock-crosswind weight-flocking]
  [set weight-flock-crosswind 0]


  set-up-wind      ;create the wind and food patch
  make-odor-plume  ;makes the odor plume

  ;Set Odor Thresholds based off of the values of the current plume (must iterate after make-odor-plume)
  set Odor-Threshold-High (mean filter [v -> v > 0] [intensity] of patches )
  set Odor-Threshold-Low ( Odor-Threshold-High  + (1 * ( standard-deviation filter [v -> v > 0] [intensity] of patches )))
  set Odor-Threshold-None ((maxintensity) + 1)

  ;initialize the birds

  ;Set parameters for each Species based on Chooser (must iterate before make-birds)
  SetSpecies1
  SetSpecies2

  make-birds ;Make the birds

end

to SetSpecies1
  if Species1 = "Black-browed albatross"
  [
    set S1-Body-Length-Meters 0.9
    set S1-Flight-Speed-MpS 11.5
  ]
  if Species1 ="Wilson's storm-petrel"
  [
    set S1-Body-Length-Meters 0.17
    set S1-Flight-Speed-MpS 9
  ]
  if Species1 ="White-chinned petrel"
  [
   set S1-Body-Length-Meters 0.55
   set S1-Flight-Speed-MpS 11.5
  ]
  if Species1 ="Wandering albatross"
  [
   set S1-Body-Length-Meters 1.2
   set S1-Flight-Speed-MpS 15
  ]

end
to SetSpecies2
  if Species2 = "Black-browed albatross"
  [
    set S2-Body-Length-Meters 0.9
    set S2-Flight-Speed-MpS 11.5
  ]
  if Species2 ="Wilson's storm-petrel"
  [
    set S2-Body-Length-Meters 0.17
    set S2-Flight-Speed-MpS 9
  ]
  if Species2 ="White-chinned petrel"
  [
   set S2-Body-Length-Meters 0.55
   set S2-Flight-Speed-MpS 11.5
  ]
  if Species2 ="Wandering albatross"
  [
   set S2-Body-Length-Meters 1.2
   set S2-Flight-Speed-MpS 15
  ]
end
to set-up-wind

  ask patches
  [
    set wind random 359 ;Random wind direction between 0 and 359
    set food 0          ;all patches start as NOT food
    ;Comment Out for Efficiency
    set pcolor white    ;all patches start out white
    set intensity 0     ;all patches start with an odor intensity of 0
  ]

  ;This has each patch set their wind direction to be the linear mean of the wind direction for all neighbors, resulting in an average wind direction of ~mod(180) with patchy noise
  ask patches [set wind mean [wind] of neighbors]

  ;Visualize the wind using the see-wind-arrows button in the GUI and uncommenting the line below (default commented out to save time during runs)
  if see-wind-arrows = True [ make-wind-arrows]

    ;make food patch such that it is 600 m from the top and from the right
  let DistToEdgePatches 600 / Patch-Scaler ;600 m in patches
  set Food-x (max-pxcor - DistToEdgePatches);((3 * max-pxcor) / 4 )        ;food patch's center x-coord is 3/4ths of the way to the right
  set Food-y (max-pycor - DistToEdgePatches );(max-pycor - (max-pycor / 8)) ;food patch's center y-coord is 1/8ths of the way from the top

   ask patch Food-x  Food-y
    [
      ask patches in-radius ((Size-Of-Food-Source-In-Meters / Patch-Scaler ) / 2) ;radius is 1/2 the diameter set by GUI
      [ set food 1
        ;Comment Out for Efficiency
        set pcolor black
      ]
  ]

end
to make-wind-arrows
  ;This algorithm cretes the arrows pointing in the wind direction
  ;;One arrow per every 10x10 patch block
  ;;Recommended to uncomment the palette extension at the beginning of code and the transparency line below (default commented out to save time during runs as palette extension can take awhile to load)
  ;;;because arrows are easier to see/look nicer when slightly transparent
  ask patches
  [if pxcor mod 10 = 0 and pycor mod 10 = 0
    [
     sprout-wa 1
      [
      set heading wind
      set color blue
      set size 6
      set shape "arrow"
      ;palette:set-transparency 70
      ]
    ]
  ]
end

to make-odor-plume
  let sourceemission 10 ;unitless
  let initialradius 6 / Patch-Scaler ;1 m -> patch
  let windspeed 15 * (1 / Patch-Scaler) * (Tick-Scaler) ;m/s -> patch / tick
  let rateofincrease (Rate-Of-Puff-Radius-Increase * (1 / Patch-Scaler) * (Tick-Scaler)) ;0.01m/s -> patch/tick
  let eddydiffusivity ((2 * sqrt ((10 ^ Eddy-Diffusivity )* Tick-Scaler) / Patch-Scaler ) );( 2 * ((10 ^ Eddy-Diffusivity) * (1 / Patch-Scaler ^ 2) * (Tick-Scaler) ) );m^2/s - >patch^2/tick


  let minyodor Food-y ; timer to determine when the first odorant reaches the bottom of the map

;Until the first odorant reaches the bottom of the map...
  while [minyodor >= (2)] ;[(any? odor with [ycor < 2]) = TRUE];[minyodor >= (2)]
  [
    ;food source sprouts odorants as a poisson process
    ask patch Food-x  Food-y
    [
      sprout-odor random-poisson (Odor-Source-Release-Rate * Tick-Scaler) ;odor/s * s/tick -> odor/tick
      [
        set t 0
        ;Comment Out for Efficiency
        set size 1
        set shape "circle"
        set color green
      ]
    ]

    ;Odors follow a biased random walk moving DOWNWIND + CROSSWIND PERTURBATION
    ask odor
    [
      set t (t + 1)
      ;Downwind
      set heading [wind] of patch-here
      forward windspeed
      ;Cross-wind perturbation (heading is drawn from a uniform distribution, amount it moves in that direction is drawn from a normal distribution of mean 0 and sd = 2sqrt(Eddy-Diffusion * delta-t) in units of patches
      set heading random 359
      let perturbation (random-normal 0 eddydiffusivity)
      forward perturbation
      if pycor <= 1 or pxcor >= (max-pxcor - 1) or pxcor <= 1 or pycor >= (max-pycor - 1)[die] ;Despawn once reach the edge of the map
    ]

    ;keep track of where the lowest odor is (as a list inclusive of max-pycor to prevent an error when there is only one odorant)
    set minyodor min (lput max-pycor ([ycor] of odor)     )
  ]


  ;odorants set their concentration as a function of their radial diffusion
  ask odor
  [
    set r_t ((initialradius + (t * rateofincrease)) )
    set size (2 * r_t); size is based on diameter, so 2*current radius - > necessary for the ask patches line
    set c_t (sourceemission * ( initialradius / (r_t)) ^ 3  ) ;Set current concentration as a function of the current radius

    ask patches in-radius r_t [set intensity ( intensity + ([c_t] of myself) ) ] ;Set the intensity of all patches you are touching (set by radius) to increase by + current Odor concentration

  ]


  ask odor [ die]



  set maxintensity max [intensity] of patches
  ;Comment Out for Efficiency
  ask patches
    [ if food != 1 and intensity != 0                               ;only update the color if it's not food and intensity isn't 0
      [set pcolor scale-color green (intensity) (maxintensity) 0]   ;set color green scaling with intensity (darker color = higher intensity).
  ]


end

to make-birds

  create-bird (N-Species1) ;create N birds (set by GUI). Generally N=50
  [
    set flightspeed (S1-Flight-Speed-MpS * (1 / Patch-Scaler) * (Tick-Scaler ) ) ;set flight speed in patches per tick
    set size  (S1-Body-Length-Meters / Patch-Scaler )                            ;set size in patches
    set detect-dist-food ( S1-Dist-Food-Meters / Patch-Scaler )                  ;set distance at which bird can directly visually detect food in patches

    ;set Olfactory Sensitivity based on the featurs of the current odor plume
    ifelse S1-Olfaction = "high"
    [set odor-intensity Odor-Threshold-High]
    [
      ifelse S1-Olfaction = "low"
      [set odor-intensity Odor-Threshold-Low]
      [set odor-intensity Odor-Threshold-None]
    ]

    set dd ((S1-DR * S1-Body-Length-Meters) / Patch-Scaler ) ;set the distance at which they can detect other birds in patches, scales with body size
    ;set the minimum distance between birds as a function of Detection Distance (and therefore body size) as DD (in meters) / Min-Sep-Denominator, then convert to patches
    set minimum-separation (((S1-DR * S1-Body-Length-Meters) / Denominator-For-Relationship-Between-DR-and-MinSep) / Patch-Scaler )
    set species 1
  ]


   create-bird (N-Species2) ;create N birds (set by GUI). Generally N=0
  [
    set flightspeed (S2-Flight-Speed-MpS * (1 / Patch-Scaler) * (Tick-Scaler ) ) ;set flight speed in patches per tick
    set size  (S2-Body-Length-Meters / Patch-Scaler )                            ;set size in patches
    set detect-dist-food ( S2-Dist-Food-Meters / Patch-Scaler )                  ;set distance at which bird can directly visually detect food in patches

    ;set Olfactory Sensitivity based on the featurs of the current odor plume
    ifelse S2-Olfaction = "high"
    [set odor-intensity Odor-Threshold-High]
    [
      ifelse S2-Olfaction = "low"
      [set odor-intensity Odor-Threshold-Low]
      [set odor-intensity Odor-Threshold-None]
    ]

    set dd ((S2-DR * S2-Body-Length-Meters) / Patch-Scaler ) ;set the distance at which they can detect other birds in patches, scales with body size
    ;set the minimum distance between birds as a function of Detection Distance (and therefore body size) as DD (in meters) / Min-Sep-Denominator, then convert to patches
    set minimum-separation (((S2-DR * S2-Body-Length-Meters) / Denominator-For-Relationship-Between-DR-and-MinSep) / Patch-Scaler )
    set species 2
  ]

  ;initial density should be 1 bird per m^2 -> A = N-birds / Patch-Scaler^2
  ;Therefore, the length of the square bounding box should be L^2=A, or L= sqrt ( N-birds) / patch-scaler in units of patches
  let startinglength (sqrt (N-birds) / (Patch-Scaler))
  let half (startinglength / 2)
  ;initial starting location is set by GUI
  let Start-X (Food-x - (Starting-Distance-From-Plume-Width-Meters / Patch-Scaler)) ;X-Coord starting location (distance from food source) in patches
  let Start-Y (Food-y - (Starting-Distance-From-Plume-Length-Meters / Patch-Scaler) );Y-Coord starting location (distance from food source) in patches

  ask bird [
    ;Set the starting location of each bird (centroid set by GUI) to be randomly located within an area of a size that allows for a density of 1bird/m^2
    setxy ((random-float startinglength - half) + Start-X ) ((random-float startinglength - half) + Start-Y )
    set heading wind - 90                              ;move crosswind to start
    set color black                                    ;default color is black
    set max-align-cohere-turn (max-align-cohere-turn-degrees-per-second * Tick-Scaler)  ;max turning angle for aligning and cohering in degrees per tick
    set max-separate-turn (max-separate-turn-degrees-per-second  * Tick-Scaler  )       ;max turning angle for separating in degrees per tick
    set max-turning-radius (max-turning-radius-degrees-per-second * Tick-Scaler)
    set food-nearby nobody                             ;no food nearby
    set flockmates no-turtles                          ;no flockmates
    set smelling-mates no-turtles                      ;no birds smelling nearby
    set nearest-neighbor no-turtles                    ;no nearest neighbor
    set did-find-food 0                                ;start without having found food (yes=1, no=0)
    set ever-find-odor 0                               ;start without ever having smelt an odor (yes=1, no=0)
    set state random 2                                 ;randomly either 1 or 0 -- direction of first zig zag can be either left or right
    set time-between-turns (Time-Per-Turn / Tick-Scaler)                                                       ;time spent maintaining heading during each zigzag (and time spent moving upwind after losing scent) in ticks
    set time-spent-olfsearch (Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds / Tick-Scaler)   ;total time spent moving upwind/zigzagging before giving up if the odor is never located again
    set timer-for-zigzag-switch time-between-turns + 1 ;this way the "zig zag heading" is set upon first running the zigzag algorithm
    set timer-since-odor time-spent-olfsearch + 1      ;did not recently encounter the odor plume
  ]

  ask bird [if pen = TRUE [pen-down]] ;Recommended on for viewing the GUI since the birds are too small to see. Recommended off for behaviorspace runs to save computational time
  ;color starting location, just for visualization purposes
  ask patches [
    if pycor <= (Start-Y + half) and pycor >= (Start-Y - half ) and pxcor <= (Start-X + half)  and pxcor >= (Start-X - half)
    [ set pcolor grey]
  ]

end

to go
  tick ;increase tick by 1

  ask bird [if xcor >= (max-pxcor - 1) or ycor >= (max-pycor - 1) [ die ]]  ;Removes agents that missed the plume. Reduces model run time

  ask bird [update-odor-timer ]  ;This updates the odor timer (or timer-since-odor). It must happen outside of the movement algorithm

  ask bird [Movement-Algorithm]  ;Main movement algorithm

  ask bird [forward flightspeed ]  ;All agents move forward at their flight-speed once the movement algorithm is complete

  ;End the model once more than (total-model-time) number of ticks have elapsed (1hr), or all of the birds have either found the food, or flown off the map
  if ticks >= (Total-Model-Time-In-Seconds / Tick-Scaler) or (sum [did-find-food] of bird) = count(bird) [stop]
end


to update-odor-timer ;This must iterate every tick BEFORE the movement algorithm is run

  ;Are you currently in an odor plume?
  ifelse ([intensity] of patch-here) >= odor-intensity
   [;if yes...
      if ever-find-odor = 0 [set allfoundodor (allfoundodor + 1)] ;Increase the AllFoundOdor master counter the FIRST time you ever found the odor
      set timer-since-odor 0                                      ;set timer-since-odor 0
      set ever-find-odor 1                                        ;mark that you found the odor plume at least once
  ]
  [;if no...
    set timer-since-odor (timer-since-odor + 1) ;add one to timer-since-odor
  ]

  ;timer for maintaining heading for the zigzags increases by 1 every tick. This ensures it will always be > time-between-turns upon first entering the zigzag algorithm
  set timer-for-zigzag-switch (timer-for-zigzag-switch + 1)



end


to Movement-Algorithm
  ;Main Movement Algorithm. An if-else chain to prioritize movement
  ;Order: Did You Already Find the Food? -> Can You Visually See Food?-> Can You Smell Food? -> Do You See Anyone Else Smelling Food? -> Did You Recently Smell Food? -> Do You Have Flockmates? -> Crosswind
  ifelse [food] of patch-here = 1 or did-find-food = 1    ;IF you found the food....
  [found-food]                                              ;...YES: stay on the food patch (and change color)
  [search-food                                              ;...NO: look for food within visual range. Outputs a list of patches within detect-dist-food that are food (0 if none)

    ifelse count food-nearby > 0                          ;IF you SEE FOOD...
    [face min-one-of food-nearby [distance myself ] ]       ;...YES: Move towards the nearest food you can see
    [                                                       ;...NO: check to see if you are in the plume

      ifelse timer-since-odor <= time-between-turns        ;IF you are IN the plume... (timer <= time-between-turns --> you continue moving upwind for one "turn-time" before you start casting)
      [in-odor-plume ]                                      ;...YES: move upwind (also changes color)
      [
        if Cuing = TRUE                               ;...NO: check to see if anyone else nearby is smelling (only runs if Cuing=TRUE, otherwise always none)
        [find-smelling]                                       ;;Outputs agents AHEAD of you, within range, that are signalling, i.e. are orange (0 if none)
        set color black                                       ;;reset color for agents who are no longer signalling ;Comment Out for Efficiency

        ifelse any? smelling-mates                        ;IF any agents nearby are signalling (always NO if Cuing = FALSE)...
          [follow-others]                                   ;...YES: move towards any signalling agents (also resets timers and changes color)
          [                                                 ;...NO: check to see if you *recently* found the plume but lost it

          ifelse timer-since-odor <= time-spent-olfsearch ;IF you encountered the plume but lost it (time-between-turns (15s) < timer < total-time-looking (300s) )...
          [zig-zag]                                         ;...YES: ZigZag
          [                                                 ;...NO: look for flockmates.
           find-flockmates                                    ;;Outputs other agents within range (0 if none)

            ifelse any? flockmates                        ;IF you have any nearby flockmates...
              [flock-crosswind]                               ;...YES: output flocking algorithm heading. If weight-flock-crosswind = 0 this is equivalent to moving crosswind, EXCEPT ALWAYS separate (don't run into each other)
              [turn-towards (wind - 90) max-turning-radius]   ;...NO: move crosswind (to the right)

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
    ;Comment Out for Efficiency
    set color green               ;color change

  ]

  set timer-since-odor 0

  ;This ensures they stay on the food patch
  ifelse [food] of patch-here = 1                     ;IF you're standing on the food patch...
  [set heading heading + random-normal 0 90]             ;...YES: move randomly
  [face one-of patches in-radius 15 with [food = 1] ]    ;...NO: set your heading to move back to the food patch

end

to search-food
  set food-nearby patches in-radius detect-dist-food with [food = 1]   ;Locate any food patches within a radius that are food
end

to in-odor-plume
  let upwind wind - 180                   ;the direction of upwind
  turn-towards upwind max-turning-radius  ; move towards upwind
  ;Comment Out for Efficiency
  set color orange                        ;color change to indicate agents that are actively smelling the plume (signal)
end

to find-smelling
  set smelling-mates other bird in-cone dd 180 with [timer-since-odor <= time-between-turns] ;Find all birds within range AHEAD of you (no backtracking) that are smelling (orange)
end

to follow-others
  ;Comment Out for Efficiency
  set color magenta   ;color change. Doesn't do anything just helps the viewer keep track of what is occurring

  ;align and cohere with signalling birds. Because this isn't weighted results in direct movement towards signalling birds within range
  align smelling-mates
  cohere smelling-mates

  set timer-since-odor (time-between-turns + 1)        ;reset timer-since-odor to 1 more than time-between-turns
  ;;this results in agents who found-a-signaler-but-then-they-stopped-signalling behave as if they found an odor but recently lost it -> i.e. zigzag

  set timer-for-zigzag-switch (time-between-turns + 1) ;reset timer for maintaining zigzag heading so that it is > time-between-turns upon first entering the zigzag algorithm (immediately zigzag)
end

to zig-zag
  ;We want to change headings every 15 s. Use time-since-odor to maintain heading between turns

  ;If you've been moving in the same direction for more than time-between-turns (15s)...
  if timer-for-zigzag-switch >= time-between-turns
  [
    set timer-for-zigzag-switch 0                 ;reset timer
    set state state + 1                           ;change heading to the opposite direction of last time
  ]

  let upwind wind - 180               ;the direction of upwind
  let zigzagheading upwind            ;new zigzag heading
  ;Set your zigzagheading to be either to the left or to the right of upwind
  ifelse state mod 2 = 0
  [ set zigzagheading ((upwind)  +  zig-zag-heading-offset) ] ;Set heading upwind + an offset (85 degrees to the right). (90 = crosswind)
  [ set zigzagheading ((upwind)  -  zig-zag-heading-offset) ] ;Set heading upwind - an offset (85 degrees to the left)

  turn-towards zigzagheading max-turning-radius ;move towards the zig-zag heading
end

to find-flockmates
  set flockmates other bird in-radius dd ;Find all birds within range
end

to flock-crosswind

  set nearest-neighbor min-one-of flockmates [distance myself] ;find nearest neighbor

  ;If you are too close to your nearest neighbor, separate, otherwise, align and cohere
  ;;After iterating, heading = flocking heading
  ifelse distance nearest-neighbor < minimum-separation
  [ separate ] ;Because this is not weighted, it results in ALL birds separating (regardless of strategy) so that no one ever runs into each other (sets new heading)

  [
    ;change heading to align, then change heading to cohere (sets new heading)
    align flockmates
    cohere flockmates

    let crosswind-heading wind - 90 ;find the crosswind heading

  ;Take the weighted mean of the flocking heading and the crosswind heading
  ;;because headings are circular, the circular average must be taken of the two headings using trig
  ;;if weight-flock-crosswind = 0 this is equivalent to setting the heading to crosswind
  ;;if weight-flock-crosswind = 1 this is equivalent to setting the heading to heading
  let mean-x  (( weight-flock-crosswind * sin heading) + ((1 - weight-flock-crosswind) * sin crosswind-heading))
  let mean-y  (( weight-flock-crosswind * cos heading) + ((1 - weight-flock-crosswind) * cos crosswind-heading))

  ;this prevents errors that occur when taking the atan of 0,0
  ifelse mean-x = 0 and mean-y = 0
    [ set heading heading  ]
    [ set heading atan mean-x mean-y ]

  ]


end

to separate
 ;this prevents the problem where if the nearest neighbor and you have the same heading you end up not changing your heading (which almost never occurs)
  ifelse heading = ([heading] of nearest-neighbor)
   [turn-away (towards nearest-neighbor + 180) max-separate-turn] ;set heading to be away from nearest neighbor (towards + 180 = away)
   [turn-away ([heading] of nearest-neighbor) max-separate-turn]  ;set heading to turn away from nearest neighbor
end

to align [mates]
  ;Get the circular average of all flockmate's headings
  let x-component sum [dx] of mates
  let y-component sum [dy] of mates
  ifelse x-component = 0 and y-component = 0
    [ set average-heading-mates heading ]
    [ set average-heading-mates atan x-component y-component ]

  turn-towards average-heading-mates max-align-cohere-turn
end

to cohere [mates]
  ;Get the circular mean heading of the direction that would take the agent towards all flockmates
  ;; "towards myself" gives us the heading from the other turtle to me, but we want the heading from me to the other turtle, so we add 180
  let x-component mean [sin (towards myself + 180)] of mates
  let y-component mean [cos (towards myself + 180)] of mates
  ifelse x-component = 0 and y-component = 0
    [ set average-heading-towards-mates heading ]
    [ set average-heading-towards-mates atan x-component y-component ]

  turn-towards average-heading-towards-mates max-align-cohere-turn
end

to turn-towards [new-heading max-turn]
  turn-at-most (subtract-headings new-heading heading) max-turn
end

to turn-away [new-heading max-turn]
  turn-at-most (subtract-headings heading new-heading) max-turn
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
491
10
950
820
-1
-1
1.0
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
450
0
800
0
0
1
ticks
30.0

BUTTON
88
13
151
46
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
163
13
226
46
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
18
530
154
563
S1-DR
S1-DR
20
200
100.0
10
1
body-lengths
HORIZONTAL

SWITCH
344
107
442
140
pen
pen
0
1
-1000

SWITCH
958
25
1158
58
see-wind-arrows
see-wind-arrows
1
1
-1000

MONITOR
962
294
1046
339
Max Intensity
maxintensity
4
1
11

SLIDER
236
13
408
46
Seed
Seed
1
500
1.0
1
1
NIL
HORIZONTAL

SLIDER
962
129
1267
162
Size-Of-Food-Source-In-Meters
Size-Of-Food-Source-In-Meters
25
100
50.0
25
1
meters in diameter
HORIZONTAL

SLIDER
19
413
235
446
S1-Body-Length-Meters
S1-Body-Length-Meters
0.1
1.5
0.9
0.1
1
m
HORIZONTAL

SLIDER
18
361
234
394
S1-Flight-Speed-MpS
S1-Flight-Speed-MpS
1
20
11.5
1
1
mps
HORIZONTAL

SLIDER
958
60
1158
93
Total-Model-Time-In-Seconds
Total-Model-Time-In-Seconds
600
3600
3600.0
30
1
s
HORIZONTAL

MONITOR
509
139
615
184
Still on Map
word (((count (bird) )/ N-birds) * 100) \" %\"
1
1
11

MONITOR
509
188
615
233
Exited Map
word (( (N-birds - count (bird) ) / N-birds) * 100) \" %\"
1
1
11

SLIDER
30
120
332
153
Starting-Distance-From-Plume-Width-Meters
Starting-Distance-From-Plume-Width-Meters
500
2000
2000.0
100
1
meters
HORIZONTAL

SLIDER
18
470
232
503
S1-Dist-Food-Meters
S1-Dist-Food-Meters
0
50
50.0
1
1
m
HORIZONTAL

SLIDER
962
369
1386
402
Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds
Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds
100
300
300.0
100
1
seconds
HORIZONTAL

SLIDER
963
407
1387
440
Time-Per-Turn
Time-Per-Turn
0
60
10.0
5
1
seconds between turns
HORIZONTAL

MONITOR
507
43
613
88
 Found Food
word ((((sum [did-find-food] of bird)) / N-birds ) * 100) \" %\"
1
1
11

SLIDER
962
589
1233
622
max-align-cohere-turn-degrees-per-second
max-align-cohere-turn-degrees-per-second
5
45
50.0
5
1
NIL
HORIZONTAL

SLIDER
962
625
1233
658
max-separate-turn-degrees-per-second
max-separate-turn-degrees-per-second
0.5
2
1.0
0.5
1
NIL
HORIZONTAL

SLIDER
30
85
332
118
Starting-Distance-From-Plume-Length-Meters
Starting-Distance-From-Plume-Length-Meters
1000
5000
3000.0
500
1
meters
HORIZONTAL

SLIDER
962
443
1388
476
zig-zag-heading-offset
zig-zag-heading-offset
0
90
40.0
5
1
NIL
HORIZONTAL

CHOOSER
18
250
233
295
Species1
Species1
"Set Manual" "Black-browed albatross" "Wilson's storm-petrel" "White-chinned petrel" "Wandering albatross"
1

CHOOSER
260
251
476
296
Species2
Species2
"Set Manual" "Black-browed albatross" "Wilson's storm-petrel" "White-chinned petrel" "Wandering albatross"
1

SLIDER
18
312
233
345
N-Species1
N-Species1
0
50
50.0
1
1
NIL
HORIZONTAL

SLIDER
261
312
476
345
N-Species2
N-Species2
0
50
0.0
1
1
NIL
HORIZONTAL

CHOOSER
18
705
230
750
S1-Olfaction
S1-Olfaction
"high" "low" "none"
1

MONITOR
1053
294
1145
339
High Olf (mean):
Odor-Threshold-High
4
1
11

MONITOR
1152
294
1266
339
Low Olf (mean + sd):
Odor-Threshold-Low
4
1
11

SLIDER
261
360
477
393
S2-Flight-Speed-MpS
S2-Flight-Speed-MpS
1
20
11.5
1
1
mps
HORIZONTAL

SLIDER
261
414
478
447
S2-Body-Length-Meters
S2-Body-Length-Meters
0.1
1.5
0.9
0.1
1
m
HORIZONTAL

SLIDER
257
471
476
504
S2-Dist-Food-Meters
S2-Dist-Food-Meters
0
50
50.0
1
1
m
HORIZONTAL

SLIDER
256
528
393
561
S2-DR
S2-DR
20
200
200.0
10
1
body-lengths
HORIZONTAL

CHOOSER
260
707
480
752
S2-Olfaction
S2-Olfaction
"high" "low" "none"
2

MONITOR
508
92
614
137
 Found Odor
word ((allfoundodor / N-birds ) * 100) \" %\"
1
1
11

SLIDER
962
553
1233
586
weight-flocking
weight-flocking
0.3
0.95
0.8
0.05
1
NIL
HORIZONTAL

SLIDER
961
480
1388
513
max-turning-radius-degrees-per-second
max-turning-radius-degrees-per-second
10
90
50.0
10
1
degrees/sec
HORIZONTAL

TEXTBOX
962
109
1112
127
Features of the Odor Plume
11
0.0
1

SLIDER
19
596
478
629
Denominator-For-Relationship-Between-DR-and-MinSep
Denominator-For-Relationship-Between-DR-and-MinSep
1
20
8.0
1
1
NIL
HORIZONTAL

MONITOR
160
526
231
571
S1 DR
word (round(S1-DR * S1-Body-Length-Meters)) \" meters\"
2
1
11

MONITOR
18
635
232
680
S1 Minimum Separation
word (round (((S1-DR * S1-Body-Length-Meters) / Denominator-For-Relationship-Between-DR-and-MinSep))) \" meters\"
2
1
11

TEXTBOX
964
535
1168
553
Features of the Flocking Algorithm
11
0.0
1

TEXTBOX
963
351
1212
379
Features of the Olfactory Search Algorithm
11
0.0
1

TEXTBOX
960
6
1215
32
Features to Initialize World and Model
11
0.0
1

TEXTBOX
74
233
224
251
Values for Species 1
11
0.0
1

TEXTBOX
143
511
375
529
distance at which they can see another bird
11
0.0
1

TEXTBOX
45
577
466
595
minimum distance (or separation) between birds (set as DD / initialization- denominator)
11
0.0
1

TEXTBOX
319
233
469
251
Values for Species 2
11
0.0
1

MONITOR
400
524
475
569
S2 DD
word (round (S2-DR * S2-Body-Length-Meters)) \" meters\"
1
1
11

MONITOR
258
633
478
678
S2 Minimum Separation
word (round((S2-DR * S2-Body-Length-Meters) / Denominator-For-Relationship-Between-DR-and-MinSep)) \" meters\"
2
1
11

TEXTBOX
190
65
340
83
Setting up the Birds:
12
0.0
1

TEXTBOX
213
298
363
316
number of birds
11
0.0
1

TEXTBOX
222
346
372
364
flight speed
11
0.0
1

TEXTBOX
224
397
374
415
body length
11
0.0
1

TEXTBOX
131
456
374
474
distance at which they can directly see the food
11
0.0
1

TEXTBOX
509
24
659
42
Model Results:
11
0.0
1

TEXTBOX
61
686
442
707
olfaction threshold -- see monitors under odor plume features for values
11
0.0
1

CHOOSER
175
174
330
219
Strategy
Strategy
"Olfaction Only" "Flock Foraging" "Local Enhancement" "Network Foraging"
1

SLIDER
962
169
1267
202
Rate-Of-Puff-Radius-Increase
Rate-Of-Puff-Radius-Increase
0.001
0.03
0.001
0.009
1
m/s
HORIZONTAL

SLIDER
962
209
1267
242
Odor-Source-Release-Rate
Odor-Source-Release-Rate
1
10
5.0
1
1
puffs/sec
HORIZONTAL

SLIDER
962
249
1267
282
Eddy-Diffusivity
Eddy-Diffusivity
-3
1
-1.0
1
1
(10 ^ __ (m^2/s))
HORIZONTAL

@#$#@#$#@
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
  <experiment name="Parameterizing_Sensitivity Analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.003"/>
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="1"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-3"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.6"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="25"/>
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="15"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="15"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="2"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="5"/>
  </experiment>
  <experiment name="Parameterizing_Odor Plume" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="25"/>
      <value value="85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="2"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="60"/>
      <value value="100"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="20"/>
  </experiment>
  <experiment name="Parameterizing_MinSep" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="20"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
      <value value="11.5"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
      <value value="12"/>
      <value value="14"/>
      <value value="16"/>
      <value value="18"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="60"/>
      <value value="100"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="20"/>
  </experiment>
  <experiment name="Parameterizing_ZigZag" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="100"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Fig 3A_BBA" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="101" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 4_BBA" repetitions="1" runMetricsEveryStep="false">
    <setup>set N-Species2 (50 - N-Species1)
setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 2])</metric>
    <metric>N-Species2</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species2">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-DR">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Olfaction">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="17"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 3B_BBA_pop 20" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 3B_BBA_pop 10" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 3B_BBA_pop 5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
  </experiment>
  <experiment name="Fig 3A_WSP" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 4_WSP" repetitions="1" runMetricsEveryStep="false">
    <setup>set N-Species2 (50 - N-Species1)
setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 2])</metric>
    <metric>N-Species2</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species2">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-DR">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Olfaction">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Flight-Speed-MpS">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Body-Length-Meters">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="17"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 3A_WCP" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 4_WCP" repetitions="1" runMetricsEveryStep="false">
    <setup>set N-Species2 (50 - N-Species1)
setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 2])</metric>
    <metric>N-Species2</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species2">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-DR">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Olfaction">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Body-Length-Meters">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="17"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 3A_Wandering Albatross" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 4_Wandering Albatross" repetitions="1" runMetricsEveryStep="false">
    <setup>set N-Species2 (50 - N-Species1)
setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 2])</metric>
    <metric>N-Species2</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species2">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-DR">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Olfaction">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Flight-Speed-MpS">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Body-Length-Meters">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="17"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="200"/>
  </experiment>
  <experiment name="Fig 3B_WSP" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="20"/>
      <value value="10"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="Fig 3B_WCP" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="20"/>
      <value value="10"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="Fig 3B_Wandering Albatross" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 0])</metric>
    <metric>(count turtles with [did-find-food = 1 and ever-find-odor = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="1.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="20"/>
      <value value="10"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="100"/>
      <value value="120"/>
      <value value="140"/>
      <value value="160"/>
      <value value="180"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;low&quot;"/>
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="Fig 4_BBA and WCP" repetitions="1" runMetricsEveryStep="false">
    <setup>set N-Species2 (50 - N-Species1)
setup</setup>
    <go>go</go>
    <metric>(count turtles with [did-find-food = 1])</metric>
    <metric>(allfoundodor)</metric>
    <metric>(count turtles)</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 1])</metric>
    <metric>(count turtles with [did-find-food = 1 and species = 2])</metric>
    <metric>N-Species2</metric>
    <enumeratedValueSet variable="pen">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="see-wind-arrows">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Model-Time-In-Seconds">
      <value value="3600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Size-Of-Food-Source-In-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Total-Time-Spent-Trying-To-Relocate-Odor-Before-Give-Up-Seconds">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Width-Meters">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Starting-Distance-From-Plume-Length-Meters">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-flocking">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Time-Per-Turn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-turning-radius-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-align-cohere-turn-degrees-per-second">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-separate-turn-degrees-per-second">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eddy-Diffusivity">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rate-Of-Puff-Radius-Increase">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Odor-Source-Release-Rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="zig-zag-heading-offset">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Denominator-For-Relationship-Between-DR-and-MinSep">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species1">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Olfaction">
      <value value="&quot;high&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-DR">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Species2">
      <value value="&quot;Set Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S1-Body-Length-Meters">
      <value value="0.55"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Flight-Speed-MpS">
      <value value="11.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Body-Length-Meters">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Dist-Food-Meters">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-DR">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S2-Olfaction">
      <value value="&quot;none&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N-Species1">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="17"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Strategy">
      <value value="&quot;Network Foraging&quot;"/>
      <value value="&quot;Olfaction Only&quot;"/>
      <value value="&quot;Flock Foraging&quot;"/>
      <value value="&quot;Local Enhancement&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Seed" first="1" step="1" last="500"/>
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
