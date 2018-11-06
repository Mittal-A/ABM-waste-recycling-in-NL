globals [
  recycling-target
  num-municipalities
  num-RC
  eta
  theta-old
  theta-single
  theta-family
  theta-couple
  TTW  ; Total waste expected to be generated from all municipalities
  investment-multiplier ; adjusts how much each knowledge factor needs to be increased by investment in knowledge
  betas-decrease-multiplier
  investment-cost ; what is the cost of each investment
]

breed [municipalities municipality]
breed [RCs RC]

undirected-link-breed [offers offer]
undirected-link-breed [contracts contract]

municipalities-own [
  num-household
  num-household-old
  num-household-single
  num-household-family
  num-household-couple
  mu ; collection infrastructure factor
  centralized? ; collection infrastructure type
  expenditure
  beta1 ; knowledge of importance of recycling
  beta2 ; knowledge of how to recycle
  TW ; collected in this month. zero at the begining of each month
  SP ; collected in this month. zero at the begining of each month
  RSP ; collected in this month. zero at the begining of each month
  remaining-waste-fraction
  price-knowledge-investment-tendency ; to see different behaviors of knowledge investment for behavior analysis
  target-knowledge-investment-tendency
  invest-in-recycling-importance
  invest-in-recycling-knowledge
  last-contract-base-price-mean
  invest-in-status-quo?
  average-recycling-rate-met
]

RCs-own[
  alpha ; technology factor
  capacity ; amount of waste can be processed monthly
  remaining-capacity ; not yet given to any municipality
]

offers-own[
  base-price ; price per waste
  total-waste ; total waste given by a municipality
  separated-waste
  recyclable-separated-waste
  m ; fine factor in our calculation
  proposed-recycling-rate ; recycling rate possible for the recycling company for a specefic municipality
  proposed-capacity ; remaining capacity of the connected RC
]

contracts-own[
  base-price ; price per waste
  promised-waste ; the waste municipality
  waste-fraction ; fraction of the municipality waste which is given to the connected
  m ; fine factor in our calculation
  recycling-rate-met ; recycling rate of last recycling done
]

to setup
  clear-all
  set-default-shape municipalities "house"
  set-default-shape RCs "factory"
  global-initialize
  municipality-initialize
  initialize-waste
  RC-initialize
  visualize
  reset-ticks
end

to global-initialize
  set recycling-target 0.5
  set num-municipalities 10
  set num-RC 5
  set eta 0.35
  set theta-old 0.3
  set theta-single 0.5
  set theta-family 1
  set theta-couple 0.8
  set TTW  0
  set investment-multiplier 0.002        ;; increase in beta1 and beta 2 with investment in knowledge
  set betas-decrease-multiplier 0.0005
  set investment-cost 20               ;;assumption that each investment in policy costs around 20000 euros

end

to municipality-initialize
  create-ordered-municipalities num-municipalities [
    set num-household (11000 + random 50000)
    set num-household-old round (0.2 * num-household)
    set num-household-single round (0.25 * num-household)
    set num-household-family round (0.30 * num-household)
    set num-household-couple round (0.25 * num-household)
    ifelse random 2 = 0
    [
      set centralized? False
      set mu (0.9 + random-float 0.1)
    ]
    [
      set centralized? True
      set mu (0.7 + random-float 0.2)
    ]
    set expenditure 0
    set beta1 (0.4 + random-float 0.1)                                  ; knowledge of importance of recycling
    set beta2 (0.3 + random-float 0.2)                                  ; knowledge of how to recycle
    set TW 0                                                            ; collected in this month. zero at the begining of each month
    set SP 0                                                            ; collected in this month. zero at the begining of each month
    set RSP 0                                                           ; collected in this month. zero at the begining of each month
    set target-knowledge-investment-tendency (0.25 + random 4 / 4)
    set price-knowledge-investment-tendency ((random 6 + 1) * 3)
    let temp random 3
    ifelse temp = 0
    [ set invest-in-recycling-importance 1
      set invest-in-recycling-knowledge 1]
    [ ifelse temp = 1
      [  set invest-in-recycling-importance 1.5
         set invest-in-recycling-knowledge 0.5]
      [  set invest-in-recycling-importance 0.5
         set invest-in-recycling-knowledge 1.5]]
    set invest-in-status-quo? one-of list true false
  ]

end

to RC-initialize
  create-ordered-RCs num-RC
  ask RCs[
    set alpha (0.4 + random 5 * 0.05)                                  ;;the sorting factor of each company is set to a random value
    set capacity (TTW / num-RC * 1.15)                                  ;;capacity is set to be an equal proportion of the total waste expected to be generated from all the municipalities
    set remaining-capacity capacity                                     ;;the remaining capacity is equal to the total capacity of the RC at the start of the model run
  ]
end

to initialize-waste
  ask municipalities [
    produce-waste 1
    set TTW (TTW + TW)
  ]
end


to go

  technology-progress
  target-change

  ask municipalities [ produce-waste ticks]

  if remainder ticks 36 = 0 [
    last-contract-history
    clear-previous-contracts
    contract-procedure
  ]

  ask RCs [process-waste] ; it should update expenditure of the related municipality by base price and fine as well

  ask municipalities [
    check-target-investment-necessity
    set label round expenditure
    decrease-betas
    check-recycling-rate
  ]

  tick
  if ticks >= 240 [stop]
end

;; general procedures:

to visualize
  ask municipalities
  [
    fd 15
    set color 10 * who + 5
    set size num-household / min [num-household] of municipalities
  ]
  ask RCs
  [
    fd 10
    set color orange
    set size 2
  ]
end

to target-change

  if (remainder ticks month-before-technology-increase = 0 and ticks != 0)
  [
    ask RCs[
      set alpha (min list (alpha + technology-increase / 100) 0.9)
    ]
  ]
end

to technology-progress
  if (remainder ticks month-before-target-increase = 0 and ticks != 0)
  [
    set recycling-target (min list (recycling-target + recycling-target-increase / 100) 0.9)
  ]
end

to clear-previous-contracts
  ask municipalities [set remaining-waste-fraction 1]
  ask contracts [die] ; removing previous contracts to build new contracts
  ask RCs [set remaining-capacity capacity]
end

to last-contract-history
  if ticks != 0 [
    ask municipalities [set last-contract-base-price-mean mean [base-price] of my-contracts]
  ]
end

to contract-procedure
  foreach sort-on [beta1 * beta2 * mu] municipalities [ the-municipality ->
    ask the-municipality [
      while [[remaining-waste-fraction] of self > 0] [
        request-offer
        ask RCs [create-offer]
        establish-contract ; municipality chooses the contract it wants and make a contract link based on that ; if an offer is established, left cacpacity of that RC should be updated
        ask offers [die] ; removing the offers before going to the next municipality
      ]
      if ticks != 0 [
        check-price-investment-necessity
      ]
    ]
  ]
end

;; municipality procedures:

to produce-waste [ month-no ]

  set TW ((waste-function month-no) * (num-household-old * theta-old + num-household-family * theta-family + num-household-family * theta-family + num-household-single * theta-single))
  set RSP TW * beta1 * mu * eta
  set SP (RSP + (1 - beta2) * (TW - RSP))

end

to-report waste-function [x]
  report ((40 - 0.04 * x - exp(-0.01 * x) * sin(0.3 * x)) / 1000000)
end

to request-offer
  create-offers-with RCs [
    set base-price 0
    set total-waste ([TW] of myself * max list [remaining-waste-fraction] of myself 0)
    set separated-waste ([SP] of myself * max list [remaining-waste-fraction] of myself 0)
    set recyclable-separated-waste ([RSP] of myself * max list [remaining-waste-fraction] of myself 0)
    set m 0
    set proposed-recycling-rate 0
    set proposed-capacity 0
  ]
end

to establish-contract
  let cheapest-base-price min ([base-price] of my-offers)
  let candidate-offer one-of my-offers
  let offer-utility 0
  let candidate-RC one-of RCs
  ask candidate-offer [
    set offer-utility ((min list recycling-target proposed-recycling-rate) / recycling-target * 2 + cheapest-base-price / base-price)
  ]
  ask my-offers[
    if offer-utility <= ((min list recycling-target proposed-recycling-rate) / recycling-target * 2 + cheapest-base-price / base-price) [
      set offer-utility ((min list recycling-target proposed-recycling-rate) / recycling-target * 2 + cheapest-base-price / base-price)
      set candidate-offer self
    ]
  ]
  ask candidate-offer[
    set candidate-RC other-end
    ask other-end
    [
      set remaining-capacity (remaining-capacity - [proposed-capacity] of myself)
    ]
  ]

  create-contract-with candidate-RC[
    set base-price [base-price] of candidate-offer
    set promised-waste [proposed-capacity] of candidate-offer
    set waste-fraction (promised-waste / [TW] of myself)
    set m [m] of candidate-offer
    set recycling-rate-met 0
    set color green
  ]

  set remaining-waste-fraction max list (remaining-waste-fraction - [waste-fraction] of contract-with candidate-RC) 0
end

to check-target-investment-necessity
  foreach [recycling-rate-met] of my-contracts [ the-rate ->
    if the-rate < recycling-target[
      invest-in-knowledge target-knowledge-investment-tendency
      stop
    ]
  ]
end

to check-price-investment-necessity
  if mean [base-price] of my-contracts >= last-contract-base-price-mean[
    invest-in-knowledge (price-knowledge-investment-tendency * mean [base-price] of my-contracts / last-contract-base-price-mean)
  ]
end

to invest-in-knowledge [tendency]
  if (beta1 < 0.9) or (beta2 < 0.9)[
    set expenditure (expenditure + investment-cost * tendency * (invest-in-recycling-importance + invest-in-recycling-knowledge))
    set beta1 min list (beta1 + invest-in-recycling-importance * investment-multiplier * tendency) 0.9
    set beta2 min list (beta2 + invest-in-recycling-knowledge * investment-multiplier * tendency) 0.9
  ]
end

to decrease-betas
  ifelse invest-in-status-quo? = true
  [ set expenditure (expenditure + investment-cost / investment-multiplier * betas-decrease-multiplier)]
  [
    set beta1 max list 0.4 (beta1 - betas-decrease-multiplier)
    set beta2 max list 0.3 (beta2 - betas-decrease-multiplier)
  ]
end

to check-recycling-rate
  set average-recycling-rate-met mean [recycling-rate-met] of my-contracts
end

;; RC procedures:

to create-offer
  let temp1 remaining-capacity
  let temp2 alpha
  let minimum-alpha min [alpha] of RCs
  ifelse remaining-capacity > 0
  [
    ask my-offers
    [
      let ersp temp2 * recyclable-separated-waste                              ;;extractable recyclable waste from recyclable separated waste
      let ernsp temp2 * temp2 * ((eta * total-waste) - recyclable-separated-waste);;extractable recyclable waste from recyclable non-separated waste
      set proposed-recycling-rate ((ersp + ernsp) / (eta * total-waste))       ;;recycling target proposed based on RCs ability to extract recyclable waste from total recyclable plastics
      set base-price ((0.9 + random-float 0.1) * 100 + max list (([alpha] of myself / minimum-alpha - (separated-waste / total-waste)  - (recyclable-separated-waste / separated-waste)) * 100) 0)
      if [centralized?] of other-end = False
      [
        set base-price base-price * 1.1                                        ;;RCs charge more base price if collection infrastructure is decentralized
      ]
      set m 1.2 + random-float 0.3                                             ;;m is a random factor (1.2, 1.5)
      set proposed-capacity min list temp1 total-waste                         ;;Update offer link with remaining capacity of the RC
    ]
  ]
  [
    ask my-offers                                                              ;;undo the offers requested from the company if there is no capacity left to be offered
    [die]
  ]
end

to process-waste
  let tech alpha
  ask my-contracts                                                               ;;for each contract in my contracts
  [
    let x ([rsp] of other-end * waste-fraction)                                  ;;local variable to store RSP of municipality with which the contract is made
    let y ([sp] of other-end * waste-fraction)                                   ;;local variable to store SP of municipality with which the contract is made
    let waste-collected ([TW] of other-end * waste-fraction)                     ;;local variable to store the waste passed on to RC for recycling
    let RNSP ((min list waste-collected promised-waste) * eta - x)
    let fine 0                                                                   ;;local variable to store fine to be collected by RC
    let money-to-be-collected 0                                                  ;;local variable to store total money to be collected by RC (fine + money for processing waste collected)

    if waste-collected < promised-waste                                          ;;should the municipality pay fine?
    [
      set fine (m * base-price *  (promised-waste - waste-collected))            ;;calculate fine =  m * max((promised - delivered), 2% of promised) * base price
    ]
    set money-to-be-collected ((waste-collected * base-price) + fine)

    set recycling-rate-met ((tech * x) + (tech * tech * RNSP)) / (waste-collected * eta) ;;update contract with this month's recycling rate

    ask other-end[                                                               ;;ask municipalities to update their expenditure based on the money to be collected by the RC
      set expenditure (expenditure + money-to-be-collected)                      ;;collect base price * waste processed in the month + fine
    ]
  ]
end

to-report municipality-stats [x]
  let money round [expenditure] of municipality x
  let rate precision ([average-recycling-rate-met] of municipality x) 2
  let importance precision ([beta1] of municipality x) 2
  let knowledge precision ([beta2] of municipality x) 2
  let met? True
  if [average-recycling-rate-met] of municipality x < recycling-target
  [ set met? False ]
  report (word money "," rate "," importance "," knowledge "," met?)
end
@#$#@#$#@
GRAPHICS-WINDOW
6
10
443
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
293
457
356
490
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
282
536
370
569
go-always
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

BUTTON
288
497
365
530
go-once
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

PLOT
522
16
722
166
Average recycling target met
NIL
NIL
0.0
250.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if count contracts > 0[\nlet x 0\nask contracts[\n set x x + recycling-rate-met\n]\nplot x / count contracts]"

SLIDER
7
455
246
488
recycling-target-increase
recycling-target-increase
0
5
5.0
1
1
percent
HORIZONTAL

SLIDER
6
494
245
527
month-before-target-increase
month-before-target-increase
12
60
48.0
1
1
NIL
HORIZONTAL

SLIDER
6
530
245
563
technology-increase
technology-increase
0
5
5.0
1
1
percent
HORIZONTAL

SLIDER
6
567
248
600
month-before-technology-increase
month-before-technology-increase
12
60
48.0
1
1
NIL
HORIZONTAL

PLOT
523
222
723
372
Average monthly mun. expenditure
NIL
NIL
0.0
250.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks != 0[\n  let x 0\n  ask municipalities[\n    set x (x + expenditure)\n  ]\n  plot x / count municipalities / ticks]"

PLOT
528
430
728
580
Average betas
NIL
NIL
0.0
250.0
0.0
1.0
true
true
"" ""
PENS
"beta1" 1.0 0 -2674135 true "" "let x 0\nask municipalities[\n set x x + beta1\n]\nplot x / count municipalities"
"beta2" 1.0 0 -13345367 true "" "let x 0\nask municipalities[\n set x x + beta2\n]\nplot x / count municipalities"

PLOT
750
272
1113
505
Municipalities beta1
NIL
NIL
0.0
250.0
0.3
1.0
true
false
"foreach [who] of municipalities[ the-who -> \ncreate-temporary-plot-pen (word the-who)\nset-current-plot-pen (word the-who)\nset-plot-pen-color 10 * the-who + 5]" "foreach sort municipalities[ the-municipality ->\n   set-current-plot-pen (word ([who] of the-municipality))\n   plot [beta1] of the-municipality\n ]"
PENS

PLOT
1122
273
1491
504
municipalities beta2
NIL
NIL
0.0
250.0
0.3
1.0
true
false
"foreach [who] of municipalities[ the-who -> \ncreate-temporary-plot-pen (word the-who)\nset-current-plot-pen (word the-who)\nset-plot-pen-color 10 * the-who + 5]" "foreach sort municipalities[ the-municipality ->\n   set-current-plot-pen (word ([who] of the-municipality))\n   plot [beta2] of the-municipality\n ]"
PENS

PLOT
747
25
1111
254
Average municipalities expenditure per month per household
NIL
NIL
0.0
250.0
0.0
8.0
true
false
"foreach [who] of municipalities[ the-who -> \ncreate-temporary-plot-pen (word the-who)\nset-current-plot-pen (word the-who)\nset-plot-pen-color 10 * the-who + 5]" "foreach sort municipalities[ the-municipality ->\n   set-current-plot-pen (word ([who] of the-municipality))\n   if ticks != 0[plot [expenditure] of the-municipality / ticks / [num-household] of the-municipality * 1000]\n ]"
PENS

PLOT
1123
26
1483
256
Average municipalities recycling rate
NIL
NIL
0.0
250.0
0.3
0.5
true
false
"foreach [who] of municipalities[ the-who -> \ncreate-temporary-plot-pen (word the-who)\nset-current-plot-pen (word the-who)\nset-plot-pen-color 10 * the-who + 5]" "foreach sort municipalities[ the-municipality ->\n   set-current-plot-pen (word ([who] of the-municipality))\n   if ticks != 0 [plot [average-recycling-rate-met] of the-municipality]\n ]"
PENS

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

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

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
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Main experiment" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>municipality-stats 0</metric>
    <metric>municipality-stats 1</metric>
    <metric>municipality-stats 2</metric>
    <metric>municipality-stats 3</metric>
    <metric>municipality-stats 4</metric>
    <metric>municipality-stats 5</metric>
    <metric>municipality-stats 6</metric>
    <metric>municipality-stats 7</metric>
    <metric>municipality-stats 8</metric>
    <metric>municipality-stats 9</metric>
    <enumeratedValueSet variable="month-before-technology-increase">
      <value value="12"/>
      <value value="24"/>
      <value value="36"/>
      <value value="48"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technology-increase">
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recycling-target-increase">
      <value value="1"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="month-before-target-increase">
      <value value="12"/>
      <value value="24"/>
      <value value="36"/>
      <value value="48"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Test experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="240"/>
    <metric>municipality-stats 0</metric>
    <metric>municipality-stats 1</metric>
    <metric>municipality-stats 2</metric>
    <metric>municipality-stats 3</metric>
    <metric>municipality-stats 4</metric>
    <metric>municipality-stats 5</metric>
    <metric>municipality-stats 6</metric>
    <metric>municipality-stats 7</metric>
    <metric>municipality-stats 8</metric>
    <metric>municipality-stats 9</metric>
    <enumeratedValueSet variable="month-before-technology-increase">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="technology-increase">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recycling-target-increase">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="month-before-target-increase">
      <value value="12"/>
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
