module Autocomplete.Update (..) where

import Autocomplete.Model exposing (Model)
import Autocomplete.Config exposing (Completed)


{-| A description of a state change
-}
type Action
  = Complete
  | ChangeSelection Int
  | ShowMenu Bool
  | UpdateItems (List String)
  | SetValue String


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Model -> ( Model, Completed )
update action model =
  case action of
    Complete ->
      let
        selectedItem =
          List.drop model.selectedItemIndex model.matches
            |> List.head
      in
        case selectedItem of
          Just item ->
            ( { model | value = item }, True )

          Nothing ->
            ( model, True )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min (model.config.maxListSize - 1)
      in
        ( { model | selectedItemIndex = boundedNewIndex }, False )

    ShowMenu bool ->
      ( { model | showMenu = bool }, False )

    UpdateItems items ->
      ( { model
          | items = items
          , matches =
              List.filter (\item -> model.config.filterFn item model.value) model.items
                |> List.sortWith model.config.compareFn
        }
      , False
      )

    SetValue value ->
      if value == "" then
        ( { model
            | value = value
            , matches =
                model.items
                  |> List.sortWith model.config.compareFn
            , selectedItemIndex = 0
          }
        , False
        )
      else
        ( { model
            | value = value
            , matches =
                List.filter (\item -> model.config.filterFn item value) model.items
                  |> List.sortWith model.config.compareFn
            , selectedItemIndex = 0
          }
        , False
        )
