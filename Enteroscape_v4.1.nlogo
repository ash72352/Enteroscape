

;extensions [array lt math r]


; v2.7 added ability to start from the same random seed to reproduce a model run
; v2.7 made Beneficial-Microbe turn a bit so look like they are moving more naturally
; v2 changed bacteria to benefical microbe -- shape and movement matches yeast
; v2 decided on proper time scaleing
; v3 added expansion and changed benefical microbes to move correctly
; v3.2 fixed spelling of Beneficial and merged redundant functions (create-microbe, move-microbe, free-microbe)
; v3.3 merged biofilm procedures
; v3.4 improved swelling physics: swelling now widens around a sufficiently sized clump of yeast rather than ahead of it, and the clump causing the swelling randomly seperates and moves alongside
;                                     the widening of the intestine as though they were embedded into the mucus and move as the mucus layer moves
; v3.45 unified turtle variables: now all breeds have adhesion and time-stuck rather than each breed having their own specifically named variable for each purpose
; v3.55 biofilm state yeast can now become unstuck with small probability (implemented, still tweaking. Clumps of biofilm yeast in the distended intestine now move 0.5 patch every 5 ticks
; v3.555 using sigmoid function to determine biofilm unstickiness
; v3.6 yeast secrete onto every patch they touch
; v3.7 changed swelling  progression; more individual and localized rather than blanket distension
; v3.71 experimental spilling over method for biofilm
; v3.72 2D representation build (WIP)
; v3.73 filled canal build
; v3.75 attempting to tweak model into properly representing beginning and end stages, not either or. also weighting towards adhearing to the beginning (user input)
; v3.8 tweaking swelling trigger algorithim to go as far as it should while maintaining proper timeline, adding death condition
; v3.81 attempted to fix bugs, broke everything
; v3.82 minor changes, tweaking death condition by adding scaling chance for death and beginning to implement probiotic
; v3.83 added probiotic agents and SM secretion
; v3.84 fixing swelling
; v3.9 new swelling function, working on spilling over functions
; v3.905 testing new class of pathogenic yeast for spilling over
; v4.1 cleaned up code  and redundancies

;global  and agent variables

;Diagonal coding
;pxcor = 0 and pycor <= 5 or
;abs pxcor = (pycor + 2) and pycor < 4 or
;abs pxcor = (pycor + 8) and pycor < 3

globals
[

  g-Move-Heading
  g-Start-Heading

  g-Mucous-Patch
  g-Lumen-Color
  g-Cell-Height
  g-Cell-Patch

  g-Bottom-Mucous-Top-Y
  g-Bottom-Mucous-Bottom-Y
  g-Top-Mucous-Top-Y
  g-Top-Mucous-Bottom-Y

  g-Seed
  g-clump-move
  Yeast-slip-prob
  Yeast-biofilm-radius

  g-dead
  g-increasing-chance
  g-natural-check
  g-death-increment
  g-c
  g-k

  g-Biofilm-Distend-Thresh
  g-Swell-Thresh
  g-Clump-Size

  g-death-tracker
  g-passed
  g-pharynx-distension






]

breed [ yeast a-yeast ]                    ;creates yeast turtle breed

breed [ Beneficial-Microbe prob ]             ;creates Beneficial-Microbe turtle breed




patches-own [
  Patch-Type                   ;creates the variable Patch-Type-- can be mucous, lumen, or cell
                               ;p-Yeast-biofilm                           ;creates the variables for Beneficial-Microbel molecule concentrations as well as both biofilm levels EDIT: commented out in case needed later
  p-SM
  ;p-Beneficial-Microbe-biofilm                 ;EDIT: commented out in case need later
  p-Biofilm

  p-ID
  p-Direction                            ; whether a patch is above or below y = 0. Patches with pycor > 0 have this set to 1. Patches with pycor < 0 have this set to -1. Patches on y = 0 have this set to 0.
]



turtles-own
[
  adhesion                          ;creates variables for  adhesion (unstuck, stuck, and biofilm) and tracks how long a microbe is stuck
  time-stuck                        ;to then become a biofilm
  move-speed
]





;Setup and Go procedures


to setup                                  ;setup procedure
  clear-all                               ;resets model and calls on the following setup procedures
  setup-globals                           ;globals, cells, muscous, & lumen
  setup-cells
  setup-Mucous
  setup-Lumen
  ask patches with [pycor != 0]
  [
    set p-Direction ( pycor / abs(pycor) )
  ]


  ifelse seed-on                          ; if the seed-on switch is set, use the number in the g-input-seed box to start the simulation
    [set g-Seed g-input-seed]
  [set g-Seed new-seed]                 ; otherwise, start with a random seed
  random-seed g-Seed
  set g-input-seed g-Seed
  reset-ticks
  (foreach (sort patches) (range count patches) [ [p n] ->
    ask p [ set p-ID n ]
  ])
  ask patches with [ Patch-Type = "Mucous" ] [ set p-Biofilm 150 ]

end



to go                                   ;go procedure
  tick                                  ;advances and updates model with each tick


  set g-clump-move ticks                                    ; counts how many ticks have passed
  Worm-Eat                              ;calls the procedures which simulate worm eating and Beneficial-Microbe molecule secretion

  ask patches [ if (p-SM > 0) [ set p-SM ( p-SM - Beneficial-Microbe-molecule-degradation-rate ) ] ] ; degrade SM concentration to reflect peristalsis and half-life
  ask Beneficial-Microbe [ secrete ]

  ask yeast [(  ifelse
    adhesion = "unstuck" [free-microbe "yeast"]                    ;if Yeast are unstuck then the free-yeast procedure is called
    adhesion = "stuck" [stuck-yeast]                     ;if Yeast are stuck then the stuck-yeast procedure is called
    adhesion = "biofilm" [Biofilm-Build "yeast"]                 ;If Yeast are biofilm then the biofilm-yeast procedure is called

  )
    push-aside
  ]



  ask Beneficial-Microbe [(  ifelse
    adhesion = "unstuck" [free-microbe "probi"]                    ;if Beneficial-Microbe are unstuck then the free-Beneficial-Microbe procedure is called
    adhesion = "stuck" [Stuck-Beneficial-Microbe]                     ;if Beneficial-Microbe are stuck then the stuck-Beneficial-Microbe procedure is called

  )]
  skim
  if (ticks mod 200 = 0)
  [
    set g-passed 0        ; reset variable to keep track of how many microbes are reaching the end
  ]







  ask patches
  [

    if Patch-type = "Lumen"
    [
      (
        ifelse
        show-conc-of = "SM"
        [
          set pcolor scale-color 109 p-SM 300 0

        ]
        show-conc-of = "biofilm"
        [
          set pcolor scale-color 35 p-Biofilm 300 0

        ]

      )
    ]

  ]                                                           ;lumen patches change color on a gradient with Beneficial-Microbel molecule levels

  if g-dead = false                                      ; if the end of the pharynx is not at the end of the worm, check to see if it needs to extends
  [
    swell-redone
    ask patches with [Patch-Type = "Lumen"]
    [

      ask neighbors4 with [Patch-Type = "Cell"] [set Patch-Type "Mucous" set pcolor green] ;; no case where lumen patches should be sharing an edge with cell patches.
                                                                                           ;; this will also fix our swelling columns such that the final patch becomes a mucous patch
      ask patches with [Patch-Type = "Mucous"]
      [

        let check neighbors4 with [Patch-Type = "Lumen" ]
        if ( count(check) > 3 ) [ set Patch-Type "Lumen" ]

      ]
      (
        ifelse                                                                               ;; make sure any all lumen patches reflect their new concentrations as their proper color gradients
        show-conc-of = "SM"
        [
          set pcolor scale-color 109 p-SM 300 0
        ]
        show-conc-of = "biofilm"
        [
          set pcolor scale-color 35 p-Biofilm 300 0
        ]

      )


    ]


  ]




  set g-dead worm-dead?
  ;view-stages ;for parameter sweeps, comment out otherwise
  if(g-dead) [
    ;user-message "The worm has died" ;; comment out for behaviour space
    set g-pharynx-distension count ( patches with [ Patch-Type = "Mucous" and abs(pycor) > 1 and pxcor < 1 ] )
    stop
  ]

end





to swell-redone


  ask patches with [Patch-Type = "Lumen"] ;; ensures no lumen patch is touching a Cell patch. In theory, a layer of mucuous should always be between the lumen and the epithelial cell,
  [


    ask neighbors4 with [Patch-Type = "Cell"] [set Patch-Type "Mucous" set pcolor green] ;; makes sure there is always a mucous patch between a Cell and Lumen patch


  ]


  let swelling []

  ask patches with [ patch-type = "Mucous" ]
  [

    let d p-Direction
    let thisx pxcor
    let thisy pycor


    let biofilm-near 0
    let nearby neighbors with [ not( patch-type = "Cell" ) ]
    ask nearby [ set biofilm-near ( p-Biofilm + biofilm-near ) ]
    let avg-biofilm ( biofilm-near / (count nearby) )
    let counthere count(turtles-here)

    if ( counthere >  g-Swell-Thresh and avg-biofilm >= ( g-Biofilm-Distend-Thresh * abs(pycor) ) and ( abs(pycor) < 9 ) )
    [
      set swelling lput p-ID swelling



    ]
  ]


  foreach swelling [
    ? ->
    let id ?

    let start one-of patches with [ p-ID = ?]  ;; should always be the distending mucous patch
    let thisx 0
    let thisy 0
    let d 0
    let b-move 0
    let SM-move 0
    let agents-move no-turtles
    let b-here -1
    let SM-here -1
    let agents-here no-turtles
    let currenty 0

    ask start [
      set thisx pxcor
      set thisy pycor
      set d p-Direction
      set Patch-Type "Lumen"
      set pcolor 59



    ]

    ask patch thisx currenty
    [

      set b-here p-Biofilm          ;; store biofilm conc already here
      set SM-here p-SM              ;; store SM conc already here
      set agents-here turtles-here  ;; store all agents already here

      set p-Biofilm ( b-here / 2 )                      ;; overwrite biofilm conc with the biofilm conc from the previous patch (0 if first)
      set p-SM 0                          ;; overwrite SM conc with SM conc from the previous patch (0 if first)

      set b-move ( b-here / 2 )                         ;; biofilm conc here becomes the conc that needs to be moved
      set SM-move SM-here                       ;; SM conc here becomes the conc that needs to be moved
      set agents-move agents-here               ;; agents already here need to be moved next

    ]


    loop
    [

      set currenty ( currenty + d )

      ask patch thisx currenty
      [
        set b-here p-Biofilm          ;; store biofilm conc already here
        set SM-here p-SM              ;; store SM conc already here
        set agents-here turtles-here  ;; store all agents already here

        set p-Biofilm b-move                      ;; overwrite biofilm conc with the biofilm conc from the previous patch (0 if first)
        set p-SM SM-move                          ;; overwrite SM conc with SM conc from the previous patch (0 if first)
        ask agents-move [ setxy xcor (ycor + d)
        ] ;; move agents from previous patch to current patch

        set b-move b-here                         ;; biofilm conc here becomes the conc that needs to be moved
        set SM-move SM-here                       ;; SM conc here becomes the conc that needs to be moved
        set agents-move agents-here               ;; agents already here need to be moved next

      ]



      if ( abs(currenty) > abs(thisy) ) [ stop ]


    ]





  ]



end







;Procedures called during Setup


to setup-globals                            ;globals setup procedure
                                            ;assigns fixed values to all global varibales & shapes to turtle breeds
  set  g-Cell-Height 9                      ;assigns the x and y cor of the mucous patch layers

  set g-Biofilm-Distend-Thresh  75
  set g-Move-Heading 10

  set-default-shape yeast "circle"
  set-default-shape Beneficial-Microbe "circle"
  set g-dead False

  set g-Bottom-Mucous-Bottom-Y  min-pycor + g-Cell-Height
  set g-Bottom-Mucous-Top-Y  min-pycor + g-Cell-Height + 0.5
  set g-Top-Mucous-Bottom-Y  max-pycor  -  g-Cell-Height - 0.5
  set g-Top-Mucous-Top-Y  max-pycor - g-Cell-Height
  set g-clump-move 0
  set g-increasing-chance random-between-int 500 800 ;base chance worm dies

  set g-Start-Heading 50
  set g-Swell-Thresh 2
  set g-Clump-Size 4
  set Yeast-slip-prob 0.26
  set Yeast-biofilm-radius 0.15
  set g-passed 0
  set g-death-increment 0
  set g-natural-check random-between-int 500 900
  set g-death-tracker -1
  set g-pharynx-distension -1
  set g-c 2.8
  set g-k 3.4



end

;12microns = 4 yeast cells tall (funnel opening)
;6 microns = 2 yeast cells (funnel depth)

to setup-Mucous                            ;mucous setup procedure
  set g-Mucous-Patch patches with            ;sets color & patch type of the mucous layer which is defined with using global variables
      [ (pycor >= g-Bottom-Mucous-Bottom-Y and pycor <= g-Bottom-Mucous-Top-Y )
        or
        (pycor >= g-Top-Mucous-Bottom-Y and pycor  <= g-Top-Mucous-Top-Y )
  ]
  ask g-Mucous-Patch [
    set pcolor 56
    set Patch-Type "Mucous"
  ]
  ask patch -20 2 [
    set pcolor 56
    set Patch-Type "Mucous"
  ]
  ask patch -20 -2 [
    set pcolor 56
    set Patch-Type "Mucous" ]



end



to setup-Lumen                           ;lumen setup procedure
  ask patches [                          ;defines the lumen to be patches between the mucous layers & assigns color
    if (Patch-Type != "Mucous" and pycor >  g-Bottom-Mucous-Top-Y and pycor <  g-Top-Mucous-Bottom-Y )
        [ set Patch-Type "Lumen" set pcolor white]

    ;sets values for Beneficial-Microbel and yeast molecule/biofilm levels at 0
    set p-SM 0
    set p-Biofilm 0
  ]
  ask patch -20 1 [
    set pcolor white
    set Patch-Type "Lumen"
  ]
  ask patch -20 -1 [
    set pcolor white
    set Patch-Type "Lumen"
  ]
end



to setup-cells                                                   ;epithelial cell setup procedure
  set g-Cell-Patch patches with [Patch-Type != "Mucous" and Patch-Type != "Lumen"]
  ask g-Cell-Patch [                                             ;defines epithelial cells as any patch not set as mucous or lumen & sets color
    set pcolor 8
    set Patch-Type "Cell"
  ]
end





;Procedures called while the worm Eats


to Worm-Eat
  if (Eat)         [                                                   ;simulates C. elegans eating by creating 1 yeast/1 Beneficial-Microbe a determined probability
                                                                       ;if not any? yeast with [xcor = min-pxcor] [                        ;worm can only eat a yeast if there is not one already there
    create-microbe Yeast-intake-prob  -20 -20  -0.5 0.5 "yeast"        ;probability value assigned by appropriate sliders

    create-microbe Beneficial-Microbe-intake-prob   min-pxcor min-pxcor g-Bottom-Mucous-Top-Y g-Top-Mucous-Bottom-Y  "probi"  ]
end



to random-position [#xmin #xmax #ymin #ymax]                           ;random-position procedure
  setxy (random-between #xmin #xmax 6) ( random-between #ymin #ymax 6)     ;sets x and y cor to random values bewteen the assigned max and min of x and y
end




to create-microbe [#prob #min-x #max-x #min-y #max-y class]          ;procedure to create a Microbe
  if (class = "probi")                                           ; if told to create a Beneficial-Microbe, uses appropriate procedure
  [
    if ((random-between 0 100 6) <= #prob)[                          ;creates Beneficial-Microbe if a random value chosen is less than or equal to the set Beneficial-Microbe intake probability
      create-Beneficial-Microbe 1 [
        random-position #min-x #max-x #min-y #max-y                 ;calls random position procedure
        set heading random-between-int (90 - g-Start-Heading) (90 + g-Start-Heading)                                        ;sets Beneficial-Microbe direction
        set color 97                                       ;sets Beneficial-Microbe color and size
        set size .7
        set adhesion "unstuck"                               ;sets Beneficial-Microbe as unstuck and time stuck at 0
        set time-stuck 0
      ]
    ]
  ]
  if ( class = "yeast" )                                           ;if told to create a yeast, uses appropriate procedure
  [
    if ((random-between 0 100 6) <= #prob) [                         ;creates yeast if a random value chosen is less than or equal to the set yeast intake probability
      create-yeast 1 [
        random-position #min-x #max-x #min-y #max-y             ;calls random position procedure
        set color  3                                        ;(dark grey) sets yeast color and size
        set size .7
        set heading random-between-int (90 - g-Start-Heading) (90 + g-Start-Heading)                                          ;need to be heading down lumen so see yeast in front of them
        set adhesion "unstuck"                                ;sets yeast as unstuck and time stuck at 0
        set time-stuck 0
      ]
    ]

  ]


end





;Yeast Procedures

to free-microbe [ class ]                                                                   ;Free microbe procedure -- already within an 'ask yeast' statement LIZ

  if (Patch-Type = "Lumen" and ( xcor >= (max-pxcor - 0.2) and xcor <= max-pxcor)) [die]  ;keeps yeast within the lumen
  let yeast-ahead one-of other yeast with [adhesion = "biofilm"] in-cone 0.5 100                      ; need to look whether yeast in front of them -- could be on same patch





  if  ( class = "yeast" ) or ( class = "probi" ) ;and ( yeast-ahead = nobody )
  [
    move-microbe class
  ]                                   ;if no yeast on patch ahead call move procedure


  let x random-between 0 100 6           ;yeast stick if a randomly chosen value is less than the assigned yeast adhesion probability

  let pB 0
  let pSM 0
  let pType ""
  ask patch-here [ set pB p-Biofilm set pSM p-SM set pType Patch-Type ]
  if ( pType = "Mucous" ) [ set pB 50 ]
  let weightB ( 1 + (pB - pSM) / 3 )

  if( class = "yeast" )
  [
    ;;if (x < (Yeast-adhesion-prob - p-SM + p-Biofilm )) and (pType = "Mucous") ;old if statement
    if (x < ( Yeast-adhesion-prob + weightB )) ;new if, initial adhesion should depend on how much biofilm is present on patch and have a high chance to adhear to mucous
    [
      set adhesion "stuck"                                  ;sets the yeast's state as stuck
      set color 44
    ]
  ]
  if (class = "probi" )
  [

    if (x < ( Beneficial-microbe-adhesion-prob + weightB ))
    [
      set adhesion "stuck"                                 ;sets bacterium state to stuck
      set color 104
    ]

  ]

end



to move-microbe [ class ]                                                  ;move procedure


  if ([ Patch-Type ] of patch-ahead 0.1  = "Mucous" ) [                                ;if yeast are in the mucous set heading to a random direction


    set heading random-between (90 - g-Move-Heading) (90 + g-Move-Heading) 6
    fd 0.1

  ]            ;set x and y to current x & current y + defined global vairable
  let yeast-ahead one-of other yeast in-cone 1 100
  if ([ Patch-Type ] of patch-ahead 0.1  = "Lumen" )
  [                                 ;if yeast in the lumen set the heading to random within range of degrees and move forward by yeast speed
    if (xcor > 20)
    [
      set g-passed (g-passed + 1)
      die
    ]


    set heading random-between-int (90 - g-Move-Heading) (90 + g-Move-Heading)
    if( class = "yeast") [ fd Yeast-Speed ]
    if( class = "probi") [ fd Beneficial-microbe-Speed ]
  ]
  ;fd Yeast-Speed  ]
  if (xcor > 20)
  [
    set g-passed (g-passed + 1)
    die
  ]


  if ([ Patch-Type ] of patch-ahead 0.1  = "Cell" ) [ setxy xcor ycor ]                ;if yeast in epithelial cells set x and y to current x and y cor
end





to redistribute [ surround amount ]


  ask surround
  [
    let currentconc p-Biofilm

    ifelse (currentconc > 100 and amount > 0.01)
    [
      set surround neighbors with [Patch-Type = "Lumen"]
      set amount ( amount / count(surround) )
      ;redistribute surround amount
    ]
    [
      set p-Biofilm (p-Biofilm + amount)

    ]


  ]



end




to Biofilm-Build [ class ]                                                     ;biofilm procedure


  if(class = "yeast" and adhesion = "biofilm")
  [
    set time-stuck (time-stuck + 1)               ;increases the yeast time stuck


    let current-conc 0
    ask patch-here
    [
      ifelse ( Patch-Type != "Mucous" )
      [
        set current-conc p-Biofilm

        ifelse ( current-conc < 250 )
        [
          set p-Biofilm (p-Biofilm + Yeast-biofilm-secretion-rate)

        ]
        [
          redistribute (neighbors4 with [Patch-Type = "Lumen" ]) (Yeast-biofilm-secretion-rate )
        ]
      ]
      [
        redistribute (neighbors4 with [Patch-Type = "Lumen" ]) (Yeast-biofilm-secretion-rate )
      ]





    ]

    let henshin false
    let ts time-stuck

    ask patch-here
    [



      if ( ts > 800 and p-Biofilm > 250 ) [ set henshin true ]

    ]
    if (henshin)
    [
      set adhesion "inactive"
      set color red
    ]




  ]




end

to secrete

  ask patch-here
  [

    ifelse (p-SM > 200)
    [

      ask neighbors with [Patch-Type = "Lumen"]
      [

        if (p-SM < 200) [set p-SM ((p-SM + (Beneficial-Microbe-molecule-secretion-rate / 4 )))]

      ]


    ]
    [
      set p-SM (p-SM + Beneficial-Microbe-molecule-secretion-rate)
    ]


  ]









end





to-report random-between [ #min #max #decimals]                          ;report procedure for random-between
  report #min + precision ( random-float (#max - #min) ) #decimals       ;reports a value equal to the min + a randomly chosen vaule greater than or equal to 0 but less than (max-min)
end

to-report random-between-int [ #min #max ]                          ;report procedure for random-between
  report #min + random (#max - #min)                      ;reports a value equal to the min + a randomly chosen vaule greater than or equal to 0 but less than (max-min)
end





;Beneficial-Microbe Procedures


to Stuck-Beneficial-Microbe                                               ;Stuck Beneficial-Microbe procedure

  set time-stuck (time-stuck + 1)             ;increases the Beneficial-Microbe time stuck
  let biof -1
  ask patch-here [ set biof p-Biofilm ]
  let x random-between-int 0 100                   ; other microbes getting stuck to them, but I don't see that represented here. Perhaps checking if should turn to biofilm first then a more general unstick check that is weighted
  if x < (( Beneficial-Microbe-deadhesion-Prob ) - biof) [
    set adhesion "unstuck"                           ;sets bacterium state as unstuck and resets their time stuck counter
    set time-stuck 0
  ]

end



to stuck-yeast                                              ;stuck yeast procedure

  set time-stuck (time-stuck + 1)               ;increases the yeast time stuck

  let nearby-stuck-yeast other yeast in-radius Yeast-biofilm-radius with [time-stuck >= 12]  ; check to see if there are any other
  let pB 0
  let pSM 0
  let pType ""
  ask patch-here [ set pB p-Biofilm set pSM p-SM set pType Patch-Type ]
  if ( pType = "Mucous" ) [ set pB 50 ]
  let weightB ( 1 + (pB - pSM) / 3 )                                                                                                        ; yeast nearby that have been stuck a while

  let nearby-unstuck-yeast other yeast in-radius 0.9     ; check to see if any unstuck yeast nearby

  let conc 0
  ask patch-here [set conc p-Biofilm]

  ifelse                 ;if yeast have been stuck as long as the set threshold and nearby yeast are stuck they become biofilm
  (time-stuck >= 12) and any? nearby-stuck-yeast
  [
    set color orange
    set adhesion "biofilm"

  ]

  [ let x random-between 0 100 6       ; if no yeast stuck nearby, see if should become unstuck
    if x < Yeast-deadhesion-Prob - weightB - time-stuck
    [
      set adhesion "unstuck"                          ;sets the yeast's state as unstuck, resets the yeast's time stuck
      set color 3
      set time-stuck 0
    ]
  ]


end






to skim ;gives topmost layer of orange yeast chance to slide off clumps

  if((g-clump-move mod 45) = 0)
  [
    let topmostLayer turtles with [adhesion = "biofilm"] with-min [abs(ycor)]

    let x random-between 0 100 6

    ask topmostLayer
    [
      let end-patch -42
      ask patch-here [set end-patch pxcor]
      let nearby-stuck-yeast other turtles in-radius 1 with [adhesion = "biofilm"]
      if((end-patch <= 19) and (x < Yeast-deadhesion-Prob))
      [
        let is-Muc FALSE
        let is-Cell FALSE
        ask patch-here [if(Patch-Type = "Mucous") [set is-Muc TRUE]]
        ask patch-ahead 0.7 [if(Patch-Type = "Cell") [set is-Cell TRUE]]
        if((xcor + 0.5) < 20 and not(is-Muc)) and not(is-Cell)
        [
          set color 44
          set adhesion "unstuck"
          set heading random-between (90 - g-Move-Heading) (90 + g-Move-Heading) 6
          forward 0.4
        ]
        ;;if((xcor + 0.5) < 20 and (is-Muc)) and not(is-Cell) [forward 0.1]

      ]
    ]

  ]

end






to-report count-biofilm

  report count(turtles-here with [adhesion = "biofilm"])

end

to-report sigmoid [ x c k ]

  let func 1 + exp( -1 * k * ( x - c) )
  report func ^ -1

end


to-report worm-dead?


  let distended? false ; is the intestine distended enough to kill the worm from stress?
  let natural? false ; random chance to die every so often


  if ticks = g-natural-check
  [
    let totalDProb Worm-Death-Probability + g-death-increment
    let rC random-between 0 1 3
    ifelse ( ( rC < totalDProb ) )  ; if the worm death probability set by the user triggers AND the current ticks mod chance = 0, the worm dies of stress or natural causes
    [

      set g-death-tracker 2
      set natural? true

    ]
    [
      set g-natural-check ( g-natural-check + random-between-int 300 600 )
      set g-death-increment ( g-death-increment + 0.005 + g-death-increment )

    ]

  ]



  if ticks = g-increasing-chance
  [
    let mucous patches with [ patch-type = "Mucous" ]
    let cumuprob 0
    ask mucous
    [

      set cumuprob ( cumuprob + ( sigmoid abs(pycor) g-c g-k ) )

    ]
    set cumuprob ( cumuprob / ( count( mucous ) ) )
    let dam ( random-between 0 1 4 )


    ifelse ( dam < cumuprob )
    [
      set g-death-tracker 1
      set distended? true

    ]
    [
      let chance random-between-int 200 500 ; amount of ticks to pass before distension is checked again
      set g-increasing-chance ( g-increasing-chance + chance )


    ]
  ]



  report ( distended? or natural? ) ;


end









to damage-check

  show 1
  show count(patches with [patch-type = "Mucous" and abs(pycor) = 1])
  show 2
  show count(patches with [patch-type = "Mucous" and abs(pycor) = 2])
  show 3
  show count(patches with [patch-type = "Mucous" and abs(pycor) = 3])
  show 4
  show count(patches with [patch-type = "Mucous" and abs(pycor) = 4])
  show "total"
  show count(patches with [patch-type = "Mucous" and abs(pycor) >= 1])

end



to push-aside

  let occupied 0
  let yval 0

  ask patch-here [set occupied count( turtles-here with [ time-stuck <= 300 ] ) set yval pycor]
  if(occupied > (g-Clump-Size)) [

    let dieroll random-between-int 1 6
    let moving (occupied - g-Clump-Size)
    ask up-to-n-of moving turtles-here with [ adhesion != "inactive" and adhesion != "biofilm"]
    [
      ifelse ( dieroll < 5 )
      [
        move-to one-of neighbors with [not(Patch-Type = "Cell") and (abs(pycor) <= abs(yval)) ]
        setxy (xcor + random-between -0.3 0.3 6) (ycor + random-between -0.3 0.3 6)
        set adhesion "unstuck"
        set color 3
        set time-stuck 0
      ]
      [
        move-to one-of neighbors with [ not(Patch-Type = "Cell") ]
        setxy (xcor + random-between -0.3 0.3 6) (ycor + random-between -0.3 0.3 6)
        set adhesion "unstuck"
        set color 3
        set time-stuck 0
      ]


    ]
  ]


end


to view-stages

  if ticks = 1000 [ export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\" behaviorspace-run-number "_stage1.png") ]
  if ticks = 2100 [ export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\" behaviorspace-run-number "_stage2.png") ]
  if ticks = 3100 [ export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\" behaviorspace-run-number "_stage3.png") ]




end

to view-stages-figs

  if ticks = 1000
  [
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage1.png")
    ask turtles [ ht ]
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage1_hidden.png")
    ask turtles [ show-turtle ]
  ]
  if ticks = 2100
  [
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage2.png")
    ask turtles [ ht ]
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage2_hidden.png")
    ask turtles [ show-turtle ]
  ]
  if ticks = 3100
  [
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage3.png")
    ask turtles [ ht ]
    export-view ( word "D:\\Thesis_Param_Sweep_Stages_Current\\stage3_hidden.png")
    ask turtles [ show-turtle ]
  ]




end
@#$#@#$#@
GRAPHICS-WINDOW
4
115
1234
749
-1
-1
29.805
1
10
1
1
1
0
0
0
1
-20
20
-10
10
1
1
1
Ticks
30.0

BUTTON
13
10
79
43
NIL
Setup
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
97
10
160
43
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
415
44
609
77
Beneficial-Microbe-intake-prob
Beneficial-Microbe-intake-prob
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
415
10
609
43
Yeast-intake-prob
Yeast-intake-prob
0
100
34.0
1
1
NIL
HORIZONTAL

SWITCH
188
10
291
43
Eat
Eat
0
1
-1000

SLIDER
784
10
971
43
Yeast-adhesion-prob
Yeast-adhesion-prob
0
100
53.0
1
1
NIL
HORIZONTAL

SLIDER
972
10
1168
43
Yeast-deadhesion-Prob
Yeast-deadhesion-Prob
0
100
36.0
1
1
NIL
HORIZONTAL

SLIDER
1170
44
1412
77
Beneficial-Microbe-molecule-secretion-rate
Beneficial-Microbe-molecule-secretion-rate
0
1
0.33
.01
1
NIL
HORIZONTAL

SLIDER
1170
10
1402
43
Yeast-biofilm-secretion-rate
Yeast-biofilm-secretion-rate
0
1
0.04
.01
1
NIL
HORIZONTAL

SLIDER
1208
78
1471
111
Beneficial-Microbe-molecule-degradation-rate
Beneficial-Microbe-molecule-degradation-rate
0
.2
0.2
.001
1
NIL
HORIZONTAL

SLIDER
784
44
971
77
Beneficial-Microbe-adhesion-prob
Beneficial-Microbe-adhesion-prob
0
100
53.0
1
1
NIL
HORIZONTAL

SLIDER
972
44
1168
77
Beneficial-Microbe-deadhesion-prob
Beneficial-Microbe-deadhesion-prob
0
100
36.0
1
1
NIL
HORIZONTAL

PLOT
1239
118
1494
337
Yeast Tracker
Time
Number of yeast
0.0
100.0
0.0
10.0
true
true
"" ""
PENS
"Unstuck" 1.0 0 -11053225 true "" "Plot count yeast with [adhesion = \"unstuck\"]\n\n"
"Stuck" 1.0 0 -4079321 true "" "Plot count yeast with [adhesion = \"stuck\"]"
"Biofilm" 1.0 0 -955883 true "" "Plot count yeast with [adhesion = \"biofilm\"]"
"Inactive" 1.0 0 -2674135 true "" "Plot count yeast with [adhesion = \"inactive\"]"

SLIDER
415
76
667
109
Worm-Death-Probability
Worm-Death-Probability
0
1
0.02
0.01
1
NIL
HORIZONTAL

SWITCH
297
11
400
44
seed-on
seed-on
0
1
-1000

INPUTBOX
178
48
284
108
g-input-seed
-1.59482221E8
1
0
Number

CHOOSER
12
51
170
96
show-conc-of
show-conc-of
"SM" "biofilm"
1

MONITOR
292
46
398
91
Microbe Counter
count(turtles)
17
1
11

BUTTON
672
79
777
112
Hide Microbes
ask turtles [ ht ]
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
780
79
892
112
Show Microbes
ask turtles [ show-turtle ]
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
988
79
1094
112
No Inoculation
set Yeast-Speed 0.59\nset Yeast-intake-prob 34\nset Yeast-adhesion-prob 53\nset Yeast-deadhesion-Prob 36\nset Yeast-biofilm-secretion-rate 0.04\nset Beneficial-Microbe-Speed 0.59\nset Beneficial-Microbe-intake-prob 0\nset Beneficial-Microbe-adhesion-prob 53\nset Beneficial-Microbe-deadhesion-Prob 36\nset Beneficial-Microbe-molecule-secretion-rate 0.33\nset Worm-Death-Probability 0.02\n
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
1097
79
1203
112
Co-Inoculation
set Yeast-Speed 0.59\nset Yeast-intake-prob 34\nset Yeast-adhesion-prob 53\nset Yeast-deadhesion-Prob 36\nset Yeast-biofilm-secretion-rate 0.04\nset Beneficial-Microbe-Speed 0.59\nset Beneficial-Microbe-intake-prob 34\nset Beneficial-Microbe-adhesion-prob 53\nset Beneficial-Microbe-deadhesion-Prob 36\nset Beneficial-Microbe-molecule-secretion-rate 0.33\nset Worm-Death-Probability 0.02\n
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
894
79
986
112
Reset Ticks
reset-ticks
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
611
10
783
43
Yeast-Speed
Yeast-Speed
0
1
0.59
0.01
1
NIL
HORIZONTAL

SLIDER
608
44
784
77
Beneficial-microbe-Speed
Beneficial-microbe-Speed
0
1
0.59
0.01
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

epithelial
false
0
Rectangle -13345367 true false 150 0 240 150
Circle -8630108 true false 165 45 60
Line -2674135 false 240 30 240 150
Line -2674135 false 150 30 150 150

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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Model Survival Curves- 8/2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="10"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves- 8/7" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="60"/>
      <value value="50"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="30"/>
      <value value="20"/>
      <value value="10"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves- 8/13" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.25"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="30"/>
      <value value="20"/>
      <value value="10"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves- 8/21" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="30"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves 2_28" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-biofilm-time-threshold">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-molecule-secretion-rate">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-intake-prob">
      <value value="0"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-deadhesion-prob">
      <value value="58"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-conc-of">
      <value value="&quot;yeast-biofilm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-adhesion-prob">
      <value value="59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-bounce-prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-input-seed">
      <value value="366669585"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Benefical-Microbe-molecule-degradation-rate">
      <value value="0.049"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves- 12/8/22" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count ticks</metric>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="30"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves 7/31" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="70"/>
      <value value="50"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="50"/>
      <value value="70"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves 4/3/23" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-secretion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Move-Heading">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-time-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-input-seed">
      <value value="-767836158"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.196"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Start-Heading">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Inflame-Thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-conc-of">
      <value value="&quot;biofilm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-move-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-bounce-prob">
      <value value="47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Clump-Size">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves 4/6/23" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-secretion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Move-Heading">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-time-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-input-seed">
      <value value="-767836158"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.196"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Start-Heading">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Inflame-Thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-conc-of">
      <value value="&quot;biofilm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-move-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-bounce-prob">
      <value value="47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Clump-Size">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Model Survival Curves- 10/5/23 (control)" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="Bacteria-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="30"/>
      <value value="50"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-degradation-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-intake-prob">
      <value value="10"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-deadhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-biofilm-time-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bacteria-molecule-secretion-rate">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Survival Curves 10/5/23 control" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-secretion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Move-Heading">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-time-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-input-seed">
      <value value="-1839821187"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.196"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Start-Heading">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Inflame-Thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-conc-of">
      <value value="&quot;biofilm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-move-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-bounce-prob">
      <value value="47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Clump-Size">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Survival Curves 10/5/23 control large sample" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-prob">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-secretion-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Move-Heading">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-slip-prob">
      <value value="26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-biofilm-time-threshold">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-on">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-input-seed">
      <value value="-1839821187"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-radius">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.196"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Start-Heading">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Inflame-Thresh">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-time-threshold">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-conc-of">
      <value value="&quot;biofilm&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clump-move-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-bounce-prob">
      <value value="47"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Clump-Size">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NI Model Survival Curves- 10/2024" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="CI Model Survival Curves- 10/2024" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Control Model Survival Curves- 10/2024" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <enumeratedValueSet variable="Eat">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NI Model Survival Curves- 10/2024 (bulk)" repetitions="10000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="CI Model Survival Curves- 10/2024 Bulk" repetitions="10000" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>g-dead = true</exitCondition>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.03"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter sweep (Yeast-biofilm-secretion-rate)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Yeast-biofilm-secretion-rate" first="0.01" step="0.01" last="0.1"/>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter sweep (adhesion/deadhesion)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Yeast-adhesion-prob" first="20" step="5" last="70"/>
    <steppedValueSet variable="Yeast-deadhesion-Prob" first="20" step="5" last="70"/>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter sweep (Worm-Death-Probability)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Worm-Death-Probability" first="0.01" step="0.01" last="0.1"/>
  </experiment>
  <experiment name="parameter sweep (g-Biofilm-Inflame-Thresh)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g-Biofilm-Inflame-Thresh" first="50" step="5" last="100"/>
    <enumeratedValueSet variable="g-c">
      <value value="2.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-k">
      <value value="3.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter sweep (g-increasing-chance)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g-increasing-chance" first="100" step="100" last="1000"/>
  </experiment>
  <experiment name="parameter sweep (sigmoid)" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="g-Biofilm-Inflame-Thresh">
      <value value="75"/>
    </enumeratedValueSet>
    <steppedValueSet variable="g-c" first="2" step="0.1" last="4"/>
    <steppedValueSet variable="g-k" first="2" step="0.1" last="4"/>
  </experiment>
  <experiment name="parameter sweep (Beneficial-Microbe-molecule-secretion-rate)" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <metric>g-death-tracker</metric>
    <metric>g-pharynx-inflammation</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yeast-biofilm-secretion-rate">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Beneficial-Microbe-molecule-secretion-rate" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Loose Sweep Model Survival NI" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="Eat">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="Yeast-Speed" first="0.2" step="0.2" last="1"/>
    <steppedValueSet variable="Yeast-intake-prob" first="10" step="20" last="100"/>
    <steppedValueSet variable="Yeast-adhesion-prob" first="10" step="10" last="80"/>
    <steppedValueSet variable="Yeast-deadhesion-Prob" first="10" step="10" last="80"/>
    <steppedValueSet variable="Yeast-biofilm-secretion-rate" first="0.01" step="0.02" last="0.1"/>
    <enumeratedValueSet variable="Beneficial-Microbe-Speed">
      <value value="0.59"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-intake-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-adhesion-prob">
      <value value="53"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-deadhesion-Prob">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-secretion-rate">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Beneficial-Microbe-molecule-degradation-rate">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Worm-Death-Probability">
      <value value="0.03"/>
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
