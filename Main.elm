import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- My Code
import Grid exposing (view)


-- View Rendering
main: Signal  Element
main =
  Signal.map view Window.dimensions


--View
view: (Int, Int) -> Element
view (w, h) = 
  container w h middle (  
    flow down [
     flow right [
        show "Input here: "
      , show "Pretend 1" 
      , show "Pretend 2" 
      , show "Pretend 3" 
      , show "Pretend 4" 
    ]
    ,Grid.view (w, h)
  ])

-- Graph plotting
-- Start with linear (x, y)
-- Then scatter (x, y)
-- Then scatter with magnitude (x, y, z)

type alias MaxMin = 
  {
    xMax: Float
    ,xMin: Float
    ,yMax:Float
    ,yMin: Float
  }

extremes: List (Float, Float) -> MaxMin
extremes ls =
  let (xs, ys) = List.unzip ls
      maximum nums= Maybe.withDefault 0 (List.maximum nums)
      minimum nums= Maybe.withDefault 0 (List.minimum nums)
  in 
    {
      xMax = maximum xs
      ,xMin = minimum xs
      ,yMax = maximum ys
      ,yMin = minimum ys
    }


