
import Html exposing (..)
import Counter exposing (..)
import Html.Events exposing (..)


--Model
type alias Model = 
  {
   topCounter: Counter.Model
   ,bottomCounter: Counter.Model
  }

init: Int -> Int -> Model
init a b =
  {topCounter = Counter.init a, bottomCounter = Counter.init b}


--Update
type Action = 
  Reset |
  Top Counter.Action |
  Bottom Counter.Action

update: Action -> Model -> Model
update action model = 
  case action of
    Reset -> init 0 0
    Top act ->
      {
        model |
          topCounter <- Counter.update act model.topCounter
      }
    Bottom act ->
      {
        model |
          bottomCounter <- Counter.update act model.bottomCounter
      }



--View

view: Signal.Address Action -> Model -> Html
view address model =
  div []
   [
     Counter.view (Signal.forwardTo address Top) model.topCounter
     ,Counter.view (Signal.forwardTo address Bottom) model.bottomCounter
     , button [onClick address Reset] [text "Reset"]
     ]

-- Wiring
main: Signal Html
main = 
  Signal.map (view actions.address) model

model: Signal Model
model = 
  Signal.foldp update (init 0 0) actions.signal

actions: Signal.Mailbox Action
actions=
  Signal.mailbox Reset
