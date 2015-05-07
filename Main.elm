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
 let trimW = ((toFloat w /5) * 4)
     trimH = ((toFloat h / 5) * 4 )
     rw = round trimW
     rh = round trimH
 in
  container w h middle (  
    flow down [
     flow right [
        show "Input here: "
      , show "Pretend 1" 
      , show "Pretend 2" 
      , show "Pretend 3" 
      , show "Pretend 4" 
    ]
    , collage rw rh [ 
       toForm (Grid.view (rw, rh))
       ,draw (scale (w, h) (extremes genPoints) genPoints ) |> move (-0.5 * (toFloat w), -0.5 * (toFloat h))
       --,draw (scale (w, h) (extremes straightLine) straightLine)  |> move (-0.5 * (toFloat w), -0.5 * (toFloat h))
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

straightLine: List Point
straightLine = 
  List.map2 (,) [-10.0..10.0] (List.map (\x -> x*x) [-10.0..10.0])

genPoints: List Point
genPoints = 
  let r = [-100.0 .. 100.0]
  in 
    List.map2 (,) r  (List.map (\x -> x * x * x) r )  


draw: List Point -> Form 
draw points = 
  traced (solid red) (path points)
