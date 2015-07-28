module Materials where

import Surfaces

purple :: Color
purple = Color 0.5 0.2 0.5
white :: Color
white = Color 1 1 1
red :: Color
red = Color 1 0.1 0.1
green :: Color
green = Color 0.1 1 0.1
blue :: Color
blue = Color 0.1 0.1 1
grey :: Color
grey = Color 0.5 0.5 0.5
darkgrey :: Color
darkgrey = Color 0.1 0.1 0.1
black :: Color
black = Color 0 0 0
yellow :: Color
yellow = Color 1 1 0.1
gold :: Color
gold = Color 1 0.842 0.1

whiteDull :: Material
whiteDull = makeMaterial white white 1 0 0 (Color 99 99 99)
whiteShiny :: Material
whiteShiny = makeMaterial white white 100 0.3 0 (Color 99 99 99)
whiteGlossy :: Material
whiteGlossy = makeMaterial white white 1000 0.6 0 (Color 99 99 99)

redDull :: Material
redDull = makeMaterial red red 10 0 0 red

greenDull :: Material
greenDull = makeMaterial green green 10 0 0 green
greenShiny :: Material
greenShiny = makeMaterial green green 1000 0.01 0 green

blueDull :: Material
blueDull = makeMaterial blue blue 10 0 0 blue

whiteMirror :: Material
whiteMirror = makeMaterial white white 10000 1 0 (Color 99 99 99)

purpleM :: Material
purpleM = makeMaterial purple purple 10000 1 0 (Color 1 1 1)

purpleDull :: Material
purpleDull = makeMaterial purple purple 10 0 0 (Color 1 1 1)

redM :: Material
redM = makeMaterial red red 10000 1.0 0 (Color 99 99 99)

greenM :: Material
greenM = makeMaterial green green 10000 1.0 0 (Color 99 99 99)

blueM :: Material
blueM = makeMaterial blue blue 10000 1 0 (Color 99 99 99)

blackM :: Material
blackM = makeMaterial black (Color 0.5 0.5 0.5) 10000 1 0 (Color 99 99 99)

darkgreyM :: Material
darkgreyM = makeMaterial darkgrey white 10 1 0 (Color 99 99 99)

greyM :: Material
greyM = makeMaterial grey grey 10000 1 0 (Color 99 99 99)

yellowM :: Material
yellowM = makeMaterial yellow yellow 10 1 0 (Color 99 99 99)

goldM :: Material
goldM = makeMaterial gold gold 10000 1 0 (Color 99 99 99)

goldD :: Material
goldD = makeMaterial gold gold 10 0 0 (Color 99 99 99)

glass :: Material
glass = makeMaterial black white 10000 1 1.5 white

diamond :: Material
diamond = makeMaterial black white 10000 1 2.4 white

greenGlass :: Material
greenGlass = makeMaterial (Color 0 1 0) (Color 0 1 0) 10000 1 1.5 (Color 99 0 99)

greenDiamond :: Material
greenDiamond = makeMaterial (Color 0 0.9 0) (Color 0 0.9 0) 10000 1 2.5 (Color 99 0 99)
