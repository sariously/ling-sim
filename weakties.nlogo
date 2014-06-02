extensions [network]
breed [speakers speaker]
breed [sites site]
undirected-link-breed [weak-ties weak-tie]
undirected-link-breed [strong-ties strong-tie]


speakers-own [
  speaker-site ; site id
  strong-tie-count
  weak-tie-count
  threshold ; normally distributed
  when-speaker-adopted ; tick when speaker adopts innovation
  innovation? ; boolean var for whether or not innovation is adopted by speaker
  innovator?
  shortest-path ;shortest path to source
 
]
sites-own [
  pop ; number of speakers to be generated at this site (based on degree)
  network-density ; network density of local network
  community-speakers ; agentset of all speakers in community
  distance-from-source ; how far is site from where the adoption originated?
  majority?
  when-majority
  avg-shortest-path
  max-weak
  max-strong
  
  ]

strong-ties-own
[tie-site]



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup procedures is a modified version of the code belonging to 
;; Wilensky, U. (2005). NetLogo Preferential Attachment model. 
;; http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. 
;; Center for Connected Learning and Computer-Based Modeling, 
;; Northwestern Institute on Complex Systems, Northwestern University, Evanston, IL.

to setup
  
  clear-all
  reset-ticks
  make-map ; create a map of sites using preferential attachment to get a degree distribution that follows a power law
  layout ; avoid sites overlapping and/or hitting the world's edges
  resize-nodes ; the more links, the bigger the node
  ;generate-population ; population based on number of links
  ask sites [
    set pop count link-neighbors * 50 + 1 ]
  clear-links ; clear links now that we have established our population sizes
              ; in this model, it is the link between speakers--not sites--that is important
  make-speakers
  ask speakers [set weak-tie-count count link-neighbors set innovation? false set when-speaker-adopted -999]
  make-strong-ties
  ask strong-ties [set color white]
  innovate
  local-network-stats
  
    
end ; of main setup procedure


to make-map ; an observer procedure
  create-sites 1 [set shape "target"  set majority? false setxy random-xcor random-ycor set community-speakers no-turtles ];; first node, unattached
  make-site site 0      ;; second node, attached to first node
  repeat num-sites - 2 [make-site find-partner ]   ;; find partner & use it as attachment point for new site
                                                         ;; do this for the number of sites desired (slider based)
  
end ; of make-map procedure


to make-site [old-site] ; an observer procedure used to create new sites
  create-sites 1
  [
    set shape "target"
    set majority? false
    set community-speakers no-turtles
    setxy random-xcor random-ycor
    if old-site != nobody
      [ create-link-with old-site 
        ;; position the new site near its partner
        ;move-to site
        ;fd 8
      ]
  ]
end

;; This code is borrowed from Lottery Example (in the Code Examples
;; section of the Models Library).
;; The idea behind the code is a bit tricky to understand.
;; Basically we take the sum of the degrees (number of connections)
;; of the turtles, and that's how many "tickets" we have in our lottery.
;; Then we pick a random "ticket" (a random number).  Then we step
;; through the turtles to figure out which node holds the winning ticket.
to-report find-partner
  let total random-float sum [count link-neighbors] of sites
  let partner nobody
  ask sites
  [
    let nc count link-neighbors
    ;; if there's no winner yet...
    if partner = nobody
    [
      ifelse nc > total
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  report partner
end

to-report find-speakpartner
  let total random-float sum [count link-neighbors] of speakers
  let speakpartner nobody
  ask speakers
  [
    let nc count link-neighbors
    ;; if there's no winner yet...
    if speakpartner = nobody
    [
      ifelse nc > total
        [ set speakpartner self ]
        [ set total total - nc ]
    ]
  ]
  report speakpartner
end


to layout ; an observer procedure that prevents sites from overlapping and hitting the edges of the world.
   
  ;; don't bump the edges of the world
  ask sites[
    
    face patch 0 0
       forward 5]
  
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
end ; of layout
  
  
to resize-nodes ; an observer procedure that resizes site nodes based on how many connections they have
                ; this will wind up visualizing relative population size
 
    ;; a node is a target with diameter determined by
    ;; the SIZE variable; using SQRT makes the target's
    ;; area proportional to its degree
    ask sites [ set size sqrt count link-neighbors ]
  
end ; of resize procedure


  


to make-speakers ; an observer procedure
  ask sites [
    hatch-speakers 1 [ ;; first node, unattached
      set shape "dot"  
      set size 1
      set threshold 0
      set speaker-site myself
      set heading random 360 
      set shortest-path -999
      fd [size] of myself * 4
      
    ]
    make-speaker one-of speakers      
    repeat pop - 2 [make-speaker find-speakpartner ]] ;; because first two have already been hatched
   
   ;; find partner & use it as attachment point for new speaker
    ;; do this for the number of speakers 
    
       
                                             
  
end ; of make-map procedure
to make-speaker [old-speaker] ; a site procedure used to create speakers
 let created count speakers with [speaker-site = myself]
 let total [pop] of self
    hatch-speakers 1
    [set shape "dot"
    set size 1
    set speaker-site myself
    if ((created / total ) <= 0.025) [set threshold 0] ; innovators
    if ((created / total ) <= 0.15) and ((created / total ) > 0.025) [set threshold 0.025] ; early adopters
    if ((created / total ) <= 0.49) and ((created / total ) > 0.15)  [set threshold 0.15] ; early majority
    if ((created / total ) <= 0.84) and ((created / total ) > 0.49) [set threshold 0.49] ; late majority
    if ((created / total ) <= 1.0) and ((created / total ) > 0.84) [set threshold 0.84] ; laggards
    
    if old-speaker != nobody
      [ create-weak-tie-with old-speaker 
        ;; position the new speaker near its partner
        set heading random 360 
        fd [size] of myself * 4
      ]
  ]
  
end

to make-strong-ties

  ask speakers 
  [ let strong-tie-limit random-poisson 2
    set strong-tie-count 0
    repeat strong-tie-limit 
    [let potential-strong-ties other speakers with [speaker-site = [speaker-site] of myself]
     if any? potential-strong-ties with [strong-tie-count < strong-tie-limit]  
     
       [set potential-strong-ties potential-strong-ties with [strong-tie-count < strong-tie-limit]
        create-strong-tie-with one-of potential-strong-ties 
        [set tie-site [speaker-site] of myself]
        set strong-tie-count strong-tie-count + 1]
      
  ]       
]

    
end

to local-network-stats
  ask sites [
    set community-speakers speakers with [speaker-site = myself] 
    let possible-connections (pop * (pop - 1)) / 2
    let actual-connections count strong-ties with [tie-site = myself]
    set network-density (actual-connections / possible-connections)
    set max-weak [weak-tie-count] of one-of community-speakers with-max [weak-tie-count]
    set max-strong [strong-tie-count] of one-of community-speakers with-max [strong-tie-count]
    ifelse any? community-speakers with [shortest-path < 0]
     [set avg-shortest-path mean [shortest-path] of community-speakers]
     [set avg-shortest-path -999]]
  
  
  
end

to innovate ; a speaker procedure
  let the-innovator one-of speakers
  ask speakers [set shortest-path  network:link-distance the-innovator links]
  ask the-innovator [set innovation? true
    set shape "box"
    set when-speaker-adopted 0
    set innovator? True
    set innovation? True
]

  ; one random speaker will be the innovator
   
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;



to go
 
 ;let end-count count sites with [majority?] ;;end simulation if all sites adopted change
 ;if end-count = count sites
 tick
 if ticks >= (count sites * 1.5) 
 [ let title random-float 1.0
  
  export-world (word "world_" num-sites title 1.0 ".csv")
  stop]
 

 ask speakers with [innovation? = true] [communicate]
 
 
end ; of go procedure






to communicate ; a speaker procedure
  if any? strong-tie-neighbors with [innovation? = false]
   [ask strong-tie-neighbors with [innovation? = false] 
     [ evaluate ]] ; evaluate whether to adopt  
  ifelse any? weak-tie-neighbors with [innovation? = false]
    [ask one-of weak-tie-neighbors with [innovation? = false] [
        evaluate]]
    [stop]
   
end ; of communicate procedure




to evaluate



  
  ;;for local threshold methods of adoption
  let threshold-number (threshold * (count link-neighbors)) 
  let tie-saturation count link-neighbors with [innovation?]
  if threshold-number <= tie-saturation
   [adopt]
   
end ;of evaluate procedure


to adopt ;; a speaker procedure
   set innovation? true
   set shape "box"
   set when-speaker-adopted ticks
   check-sites
end ;;end of adopt procedure


to check-sites

ask sites[
  if majority? = false[
   let speakers-with-innovation count no-turtles 
   if any? community-speakers with [innovation? = true]
   [set speakers-with-innovation count community-speakers with [innovation? = true]]

   
   if speakers-with-innovation >= (0.50 * pop)
      [set majority? true
       set shape "star"
       set when-majority ticks]]
  ]
  
end ;of check-sites procedure




 
@#$#@#$#@
GRAPHICS-WINDOW
278
10
1098
851
40
40
10.0
1
10
1
1
1
0
0
0
1
-40
40
-40
40
1
1
1
ticks
30.0

BUTTON
7
20
73
53
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
94
59
171
92
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

BUTTON
7
59
92
92
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
94
15
266
48
num-sites
num-sites
10
100
90
1
1
NIL
HORIZONTAL

PLOT
28
128
228
278
Population distribution
population
# of sites
10.0
500.0
0.0
20.0
true
false
"" ""
PENS
"population-hist" 1.0 0 -7858858 true "histogram [pop] of sites" ""

PLOT
38
307
238
457
Adoption of linguistic innovation
ticks
# of adopters
0.0
20.0
0.0
20.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count speakers with [innovation?]"

PLOT
50
498
250
648
Sites reaching majority
ticks
# sites
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"site-wide-adoption" 1.0 0 -16777216 true "" "plot count sites with [majority?]"

PLOT
71
663
271
813
Histogram of speaker adoption times
Tick
# of Speakers
0.0
30.0
0.0
20.0
true
false
"" ""
PENS
"adoption-speakers" 1.0 0 -16777216 true "" "histogram [when-speaker-adopted] of speakers"

@#$#@#$#@
#ODD Description of Agent-based model of language change
This file describes the model presented in the thesis of Sara Kazemi constructed in partial completion of the Master's degree program for Computational Linguistics at San Diego State University.

##Purpose
Language change has been observed to diffuse hierarchically to larger cities before spreading to smaller cities (the Cascade and Gravity models) but has also been observed to spread geographically (the Neighborhood Effect). This model addresses the role of social space and structure in spreading innovation and explores how varying social network size and density impacts the pattern in which language changes diffuse. 

##Entities, State Variables, and Scales
The model has four kinds of entities: square patches of land, speakers, links between speakers, and collectives of speakers.

###Patches
The artificial world used for this simulation is an 81 x 81 grid; there is no distance scale used for this simulation so all distances are reported in generic "units." Likewise, there is no specific time scale imposed as we are not so much concerned with the length of time language change might take, rather than the order in which it happens. Therefore, all times remain measured in "ticks."


###Speakers
Speakers have five state variables, speaker-site, strong-tie-count, weak-tie-count, innovation?, and resistance. The state variable speaker-site encodes which collective a speaker belongs to. As one might expect, strong-tie-count is simply the sum of strong ties a speaker has, while weak-tie-count is the sum of weak-ties a speaker has. The boolean state variable innovation? represents whether or not a speaker has adopted the linguistic innovation (a "true" value indicates that the speaker has adopted the innvation while a "false" value indicates that the speaker has not adopted the innovation). 

###Links

There are two breeds of links--weak-ties and strong-ties. Both breeds encode which two speakers are on either end of the link. Additionally, strong-ties have a tie-site variable, which keeps track of which local network (i.e. which collective) the tie is in. This is instrumental in determining the network density of each collective.

###Collectives

Collectives each have a population (named pop) state variable. In accordance with extant research (Gabaix, 1999), the distribution of population values follows Zipf's Law. The population values are used during the generation of speakers at each collective. Collectives also have a network density state, a value computed as the proportion of actual ties present within the local network of speakers to the number of possible ties present within the local network of speakers. The number of possible ties is calculated as: N*(N-1)/2, where N represents the number of nodes (speakers) in the network.

##Input Data
The number of collectives generated during initialization is controlled by a slider that can be manipulated the user to create 10-100 collectives.

##Initialization
The simulation is initialized by generating a random graph of the specified number of collectives on an 81 x 81 grid. Each new collective is linked to an older collective via preferential attachment, which results in a high number of collectives with one connection, and few collectives with many connections--a distribution which follows Zipf's Law. The population variable of each site is derived from the degree of connections that site has so that the population distribution of the graph will likewise follow Zipf's Law. After each site records its population value, the links between all sites are deleted as they are not used for any purpose beyond establishing population values.

The population values of each collective are then used to generate speakers at each site and the speakers encode which collective they belong to. Each speaker starts out with their boolean innovation? variable set to "false." As speakers are generated, they form weak-ties with other speakers through preferential attachment. This results in weak-ties both within and across local networks. Strong-ties between speakers are generated randomly within local networks, each speaker being limited a random number of strong-ties drawn from a Poisson distribution with a mean of 2. The network density of each collective is then computed.

Finally, one random speaker is chosen to create a linguistic innovation, which results in that speaker setting its innovation? value to "true."


##Process Overview and Scheduling

###communicate
On every tick, speakers with the innovation (the original innovator and “early adopters”) are given a chance communicate with other speakers with whom they have ties (link neighbors). Each speaker is given a 90% chance to communicate with all strong-ties and a 10% chance to communicate with weak-ties.

###adopt
If any speaker adopts the innovation, that speaker's innovation? variable is set to "true" and the speaker is allowed to communicate during the following time steps.

##Submodels

###Preferential attachment

##Design Concepts
###Basic principle
This model employs Grannovetter's strength of weak ties principle (1973, 1982), which addresses how new information or an innovation is introduced to local networks by way of weak ties with members of other more distant networks within the larger, global network.

###Interaction
Speakers interact by communicating with other speakers with whom they have ties.

###Sense
Speakers sense the strength of ties they have with others as well as a sense of collective identity based on  their collective’s network density/size.

###Adaptive Behavior
Tie strength and collective identity will contribute to how open speakers are to adopt linguistic innovation.

###Emergence
Different patterns of linguistic diffusion will emerge from varying network size/density and the way those affect each speaker’s openness/resistance to linguistic change.

###Stochasticity
Number of weak and strong ties will formed randomly with local and global speakers, but, these numbers will be determined stochastically. Need to determine the lower and upper bounds for both weak and strong ties—or perhaps we can just vary this as well.

###Goal
Speakers with low collective identity (stronger individualistic identity) want to maximize linguistic difference from the collective while speakers with high collective identity want to maintain linguistic similarity with collective.

###Prediction
Speakers predict their decision will contribute to their collective/individualistic identity.

###Observation
Need to keep track of when each speaker adopts the linguistic innovation as well as how many speakers in each collective has adopted the linguistic innovation at each tick. This will be used to analyze when mere innovation becomes a behavioral (linguistic) change.




## CREDITS AND REFERENCES

Granovetter, M. (1973). The Strength of Weak Ties. _American Journal of Sociology_. 78(_6_), 1360-1380.

Granovetter, M. (1982). The Strength of Weak Ties: A Network Theory Revisited. In P.V. Marsden and N. Lin (Eds.), _Social Structure and Network Analysis_. (pp. 105-131). Beverly Hills, CA: SAGE Publications.

Railsback, S. & Grimm, V. (2012). _Agent-based and individual-based modeling: A practical introduction_. Princeton University Press, Princeton, NJ.

Gaibax, X. (1999). Zipf's Law for Cities: An Explanation. _The Quarterly Journal of Economics_. 114(_3_), 739-767.
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
set layout? false
set plot? false
setup repeat 300 [ go ]
repeat 100 [ layout ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Thesis-runs" repetitions="5" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="num-sites">
      <value value="90"/>
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
