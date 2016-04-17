module Autocomplete.Update (..) where

import Autocomplete.Model exposing (Model)


{-| A description of a state change
-}
type Action
  = NoOp
  | Complete
  | ChangeSelection Int
  | ShowMenu Bool


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Complete ->
      let
        selectedItem =
          List.drop model.selectedItemIndex model.matches
            |> List.head
      in
        case selectedItem of
          Just item ->
            { model | value = item, showMenu = False }

          Nothing ->
            model

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min (model.config.maxListSize - 1)
      in
        { model | selectedItemIndex = boundedNewIndex }

    ShowMenu bool ->
      { model | showMenu = bool }
