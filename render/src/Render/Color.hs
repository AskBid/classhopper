module Render.Color where 

import Linear.V4

newtype Color = Color (V4 Float)

black :: Color
black = Color $ V4 0.0 0.0 0.0 1.0

greyGrid :: Color 
greyGrid = Color $ V4 0.55 0.55 0.54 0.5

green :: Color
green = Color $ V4 0.0 0.6 0.1 1.0

blue :: Color
blue = Color $ V4 0.0 0.1 0.8 1.0

red :: Color 
red = Color $ V4 9.0 0.1 0.1 1.0

blueGrid :: Color
blueGrid = Color $ V4 0.0 0.06 0.7 1.0

redGrid :: Color 
redGrid = Color $ V4 0.7 0.06 0.06 1.0

greenGrid :: Color
greenGrid = Color $ V4 0.0 0.4 0.06 1.0

