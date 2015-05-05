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
    , collage w h [
       toForm (Grid.view (w, h))
       ,draw (scale (w, h) (extremes genPoints) genPoints )
      ]
  ])

-- Graph plotting
-- Start with linear (x, y)
-- Then scatter (x, y)
-- Then scatter with magnitude (x, y, z)



-- Business Logic
type alias MaxMin = 
  {
    xMax: Float
    ,xMin: Float
    ,yMax:Float
    ,yMin: Float
  }

type alias Point = (Float, Float)

extremes: List Point -> MaxMin
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

scale: (Int, Int) -> MaxMin -> List Point -> List Point 
scale (w, h) mm points = 
  let fit i max min val = i * abs(val - min) / abs(max - min)
      f (x, y) = (fit (toFloat w) mm.xMax mm.xMin x, fit (toFloat h) mm.yMax mm.yMin y)
  in
     List.map f points

genPoints: List Point
genPoints = 
  let r = [-100.0 .. 100.0]
  in 
    List.map2 (,) (List.map (\x -> x*x) r ) r


draw: List Point -> Form 
draw points = 
  traced (solid red) (path points)
