import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main: Signal  Element
main =
  Signal.map view Window.dimensions

view: (Int, Int) -> Element
view (w, h) = 
  container w h middle (
    collage w h [
      quadrent w h X      
      , quadrent w h Y
      , toForm (majors w h Y)
    ] 
  )

line: Path
line =
  path [ (50, 50), (50, -50)]

type Axis = 
  X |
  Y

quadrent: Int -> Int -> Axis -> Form
quadrent width height axis=
  traced (solid black) (fancyLine 0 (toFloat width, toFloat height) axis)


majors: Int -> Int -> Axis -> Element
majors width height axis = 
  let range = [-3.0..3.0]
      ln i = case axis of
        X -> traced (dashed black) (fancyLine (toFloat width/i) (toFloat width, toFloat height) axis)
        Y -> traced (dashed black) (fancyLine (toFloat height/i) (toFloat width, toFloat height) axis)
  in collage width height ( List.map ln range )


fancyLine: Float -> (Float, Float) -> Axis -> Path
fancyLine n (width, height) axis = 
  case axis of
    X -> path [(-width/2, n), (width/2, n)]
    Y -> path [(n, -height/2), (n, height/2)]

