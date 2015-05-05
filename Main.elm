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
  Signal.map Grid.view Window.dimensions

