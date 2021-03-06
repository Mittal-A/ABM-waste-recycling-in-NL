globals [
  recycling-target
  num-municipalities
  num-RC
  eta
  theta-old
  theta-single
  theta-family
  theta-couple
  week ; week number
  TTW  ; Total waste expected to be generated from all municipalities
  investment-multiplier ; new line ; adjusts how much each knowledge factor needs to be increased by investment in knowledge
  investment-cost ;new line ; what is the cost of each investment
  offer-utility ; new line ; to see how good each new contract is
  cheapest-base-price ; new line ; to know cheapest base price
  candidate-offer ; new line ; to know which offer has the highest utility
  candidate-RC ; new line ; to know who has offered the candidate-offer
]

breed [municipalities municipality]
breed [RCs RC]

undirected-link-breed [offers offer]
undirected-link-breed [contracts contract]

municipalities-own [
  total-population
  pop-old
  pop-single
  pop-family
  pop-couple
  mu ; collection infrastructure factor
  centralized? ; collection infrastructure type
  expenditure
  beta1 ; knowledge of importance of recycling
  beta2 ; knowledge of how to recycle
  TW ; collected in this month. zero at the begining of each month
  SP ; collected in this month. zero at the begining of each month
  RSP ; collected in this month. zero at the begining of each month
  remaining-waste-fraction

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

  global-initialize
  municipality-initialize
  RC-initialize
  visualize

  reset-ticks
end

to global-initialize
  set recycling-target 0.65
  set num-municipalities 5
  set num-RC 10
  set eta 0.5
  set theta-old 0.15
  set theta-single 0.30
  set theta-family 0.33
  set theta-couple 0.22
  set week 0; week number
  repeat 4 [
    set week (week + 1)
    ask municipalities [
      produce-waste week
      set TTW (TTW + TW)
    ]
  ]
  set week 0                                                            ; Total waste expected to be generated from all municipalities
end

to municipality-initialize
  create-ordered-municipalities num-municipalities
  ask municipalities [
    set total-population (180000 + random 560000)
    set pop-old theta-old * total-population
    set pop-single theta-single * total-population
    set pop-family theta-family * total-population
    set pop-couple theta-couple * total-population
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
    set beta1 (0.3 + random-float 0.1)                                        ; knowledge of importance of recycling
    set beta2 (0.2 + random-float 0.2)                                        ; knowledge of how to recycle
    set TW 0                                                            ; collected in this month. zero at the begining of each month
    set SP 0                                                            ; collected in this month. zero at the begining of each month
    set RSP 0                                                           ; collected in this month. zero at the begining of each month
  ]
end

to RC-initialize
  create-ordered-RCs num-RC
  ask RCs[
    set alpha (0.4 + random 0.4)                                        ;;the sorting factor of each company is set to a random value between 0.4 and 0.8
    set capacity (TTW / num-RC)                                         ;;capacity is set to be an equal proportion of the total waste expected to be generated from all the municipalities
    set remaining-capacity capacity                                     ;;the remaining capacity is equal to the total capacity of the RC at the start of the model run
  ]
end


to go
  ask municipalities [ ;new line
    set TW 0 ;new line
    set SP 0 ;new line
    set RSP 0 ;new line
  ] ;new line
  repeat 4 [
    set week (week + 1)
    ask municipalities [ produce-waste week ]
  ]
  if remainder ticks 36 = 0 [
    ask municipalities [set remaining-waste-fraction 1]
    ask contracts [die] ; removing previous contracts to build new contracts
    ask RCs [set remaining-capacity capacity]
    foreach sort-on [beta1 * beta2 * mu] municipalities [ the-municipality ->
      ask the-municipality [
        while [[remaining-waste-fraction] of self > 0] [
          request-offer
          ask RCs [create-offer]
          establish-contract ; municipality chooses the contract it wants and make a contract link based on that ; if an offer is established, left cacpacity of that RC should be updated
          ask offers [die] ; removing the offers before going to the next municipality
        ]
      ]
    ]
  ]
  ask RCs [process-waste] ; it should update expenditure of the related municipality by base price and fine as well
  ask municipalities [check-investment-necessity]

  tick
end

;; general procedures
to visualize
  set-default-shape municipalities "house"
  set-default-shape RCs "circle"
  ask municipalities
  [
    fd 15
    set color blue
  ]
  ask RCs
  [
    fd 10
    set color orange
  ]
end

;; municipality procedures:
to produce-waste [ week-no ] ; municipality command
  set TW (TW + (waste-function week-no) * (pop-old * theta-old + pop-family * theta-family + pop-family * theta-family + pop-single * theta-single))
  set SP TW * beta1 * mu
  set RSP min (list (beta2 * SP) (eta * TW))
end

to-report waste-function [x]
  report 40 - 0.04 * x - exp(-0.01 * x) * sin(0.3 * x)
end

to request-offer ; municipality command
  create-offers-with RCs [
    set base-price 0
    set total-waste [TW] of myself
    set separated-waste [SP] of myself
    set recyclable-separated-waste [RSP] of myself
    set m 0
    set proposed-recycling-rate 0
    set proposed-capacity 0
  ]
end

to establish-contract ; municipality command
  set cheapest-base-price min ([base-price] of my-offers)
  set candidate-offer one-of my-offers
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
  ]
  create-contract-with candidate-RC[
    set base-price [base-price] of candidate-offer
    set promised-waste [proposed-capacity] of candidate-offer
    set waste-fraction (promised-waste / [TW] of myself)
    set m [m] of candidate-offer
    set recycling-rate-met 0
  ]
  set remaining-waste-fraction (remaining-waste-fraction - [waste-fraction] of contract-with candidate-RC)
end

to check-investment-necessity
  foreach [recycling-rate-met] of my-contracts [ the-rate ->
    if the-rate < recycling-target[
      invest-in-knowledge
      stop
    ]
  ]
end

to invest-in-knowledge
  set expenditure (expenditure + investment-cost)
  set beta1 (beta1 + investment-importance * investment-multiplier)
  set beta2 (beta2 + investment-knowledge-recycling * investment-multiplier)
end

;; RC procedures:
to create-offer ;RC command
    let temp1 remaining-capacity
    let temp2 alpha
    ifelse remaining-capacity = 0
    [
      ask my-offers
      [
        let ersp temp2 * recyclable-separated-waste                              ;;extractable recyclable waste from recyclable separated waste
        let ernsp temp2 * temp2 * (total-waste - separated-waste)                ;;extractable recyclable waste from non-separated waste
        set proposed-recycling-rate ((ersp + ernsp) / total-waste)               ;;recycling target proposed based on RCs ability to extract recyclable waste from total waste
        set base-price ((0.9 + random 0.2) * 50 + (3 * temp2 - (separated-waste / total-waste)  - (recyclable-separated-waste / separated-waste)) * 50) ;;fixed cost plus vairable cost
        ;;perhaps something from collection type should also be included here. If yes, it has to be added as part of the offer requested from municipality
        set m 1.2 + random 0.3                                                   ;;m is a random factor (1.2, 1.5)
        set proposed-capacity temp1                                              ;;Update offer link with remaining capacity of the RC
      ]
    ]
    [
      ask my-offers                                                              ;;undo the offers requested from the company if there is no capacity left to be offered
      [die]
    ]
end

to process-waste ;RC command
  let tech alpha
  ask my-contracts                                                               ;;for each contract in my contracts
  [
    let x [rsp] of other-end                                                     ;;local variable to store RSP of municipality with which the contract is made
    let y [sp] of other-end                                                      ;;local variable to store SP of municipality with which the contract is made
    let waste-generated [TW] of other-end                                        ;;local variable to store the total waste generated by the municipality this month
    let waste-collected (waste-generated * waste-fraction)                       ;;local variable to store the waste passed on to RC for recycling
    let fine 0                                                                   ;;local variable to store fine to be collected by RC
    let money-to-be-collected 0                                                  ;;local variable to store total money to be collected by RC (fine + money for processing waste collected)

    if waste-collected < promised-waste                                          ;;should the municipality pay fine?
    [
      set fine (m * base-price *  (promised-waste - waste-collected))            ;;calculate fine =  m * max((promised - delivered), 2% of promised) * base price
    ]
    set money-to-be-collected ((waste-collected * waste-fraction * base-price) + fine)

    set recycling-rate-met ((tech * x) + (tech * tech * y)) / waste-generated    ;;update contract with this month's recycling rate

    ask other-end[                                                               ;;ask municipalities to update their expenditure based on the money to be collected by the RC
      let present-expenditure expenditure
      set expenditure ( present-expenditure + money-to-be-collected)             ;;collect base price * waste processed in the month + fine
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
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
14
53
77
86
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
99
98
187
131
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
6
97
83
130
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

SLIDER
698
58
875
91
investment-importance
investment-importance
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
729
106
956
139
investment-knowledge-recycling
investment-knowledge-recycling
0
10
5.0
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
NetLogo 6.0.4
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
