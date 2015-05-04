module CounterList where

import Counter exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--Model
type alias Model = 
  { counters: List (ID, Counter.Model)
  , nextId: ID
  }

type alias ID = Int

init: List (Int, Int) -> Model
init ls = 
  let toModel (id, increment) = 
    (id, (Counter.init increment))
  in
   { counters= List.map toModel ls, 
     nextId = (List.foldr (\(i, a) acc -> if i > acc then i else acc) 0 ls) +1
   }


--Update

type Action = 
  Add |
  Remove|
  Modify ID Counter.Action

update: Action -> Model -> Model
update action model = 
  case action of
    Add -> 
      let newCounter = (model.nextId, Counter.init 0)
          newCounters = model.counters ++ [newCounter]
      in
         { model |
           counters <- newCounters,
           nextId <- model.nextId + 1
         }
    Remove ->
      {model | counters <- List.drop 1 model.counters } 
    
   --define a temporary function here that takes an id and a model & returns either the current value or a modified version of it if the id matches properly
    Modify id act ->
      let updateCounter (counterId, counterModel) =
          if counterId == id
           then (counterId, Counter.update act counterModel)
           else (counterId, counterModel)
      in
         {model | counters <- List.map updateCounter model.counters }
     

--View
view: Signal.Address Action -> Model -> Html
view address model = 
  let counters = List.map (viewCounter address) model.counters
      remove = button [ onClick address Remove] [text "Remove"]
      add = button [onClick address Add] [text "Add"]
  in
     div [] ([remove, add] ++ counters)

viewCounter: Signal.Address Action -> (ID, Counter.Model) -> Html
viewCounter address (id, model) = 
  Counter.view (Signal.forwardTo address (Modify id)) model

--Wiring
main: Signal Html
main = 
  Signal.map (view actions.address) model

model: Signal Model
model = 
  Signal.foldp update (init [(98, 1000)]) actions.signal

actions: Signal.Mailbox Action
actions = 
  Signal.mailbox Add


