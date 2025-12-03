module Render.Color where 

import Linear.V4

newtype Color = Color (V4 Float)

black :: Color
black = Color $ V4 0.0 0.0 0.0 0.0

green :: Color
green = Color $ V4 0.0 0.6 0.1 0.0

blue :: Color
blue = Color $ V4 0.0 0.1 0.8 0.0
