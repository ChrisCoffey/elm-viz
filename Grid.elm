module Grid(Model, init, Action, update, view)  where
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model
type alias Model = 
  {length: Int, height: Int, style: GridStyle, layout: GridLayout}

type GridStyle = 
  Basic |
  Major|
  Minor 

type GridLayout = 
  Quad |
  XY

init: Int -> Int -> Model
init len h = 
  {
    length = len
    ,height = h
    ,style = Basic
    ,layout = Quad
  }

-- Update
type Action = 
 ChangeStyle GridStyle|
 ChangeLayout GridLayout|
 ChangeScale Int Int |
 NoOp

update: Action -> Model -> Model
update action model = 
  case action of
    ChangeStyle s ->
      {model | style <- s }
    ChangeLayout l ->
      { model | layout <- l }
    ChangeScale l w ->
      { model |
        length <- l
        , height <- w
      }
    NoOp -> model

-- View

view: Signal.Address Action -> Model -> Html
view address model = 
  let basic = button [onClick address (ChangeStyle Basic) ] [text "Basic"]
      major = button [onClick address (ChangeStyle Major) ] [text "Major"]
      minor = button [onClick address (ChangeStyle Minor) ] [text "Minor"]
      quad  = button [onClick address (ChangeLayout Quad) ] [text "Grid"]
      xy    = button [onClick address (ChangeLayout XY)   ] [text "Chart"]
      -- todo update this with length and height modifications
  in
     div [] ([
        div []([basic, major, minor])
      , div []([quad, xy])
     ])


