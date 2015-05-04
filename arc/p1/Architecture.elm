module Counter where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

{--
All Elm programs are broken up in classic mvc-esque

There is a model, which contains state & logic
An Update/ Action section that is essentially the state transition table between inputs and the model
The View, which displays a given application model

Each piece is necessarily separate & free to grow independently or be reimplemented

--}

-- Model
type alias Model = Int                  


-- Update
type Action = Increment | Decrement

update : Action -> Model -> Model
update action model = 
  case action of
    Increment -> model + 1
    Decrement -> model - 1

-- View
view: Signal.Address Action -> Model -> Html
view address model = 
  div []
    [
      button [ onClick address Decrement ] [text "-"]
      ,div [ countStyle ] [text (toString model) ]
      , button [onClick address Increment ] [ text "+"]
      ]

countStyle: Attribute
countStyle = 
  style 
  [
    ("font-size", "20px")
    ,("font-family", "monospace")
    ,("display", "inline-block")
    ,("width", "50px")
    ,("text-align", "center")
    ]

-- Signals
main: Signal Html
main = 
  Signal.map (view actions.address) model

model: Signal Model
model=
  Signal.foldp update 0 actions.signal

actions: Signal.Mailbox Action
actions = 
  Signal.mailbox Increment
