module Main (..) where

import Autocomplete.Simple as Autocomplete exposing (initWithConfig, update, view)
import Autocomplete.Styling as Styling
import StartApp.Simple
import Html
import Html.Attributes exposing (class)


testData : List String
testData =
  [ "elm"
  , "makes"
  , "coding"
  , "life"
  , "easy"
  ]


styleView : Styling.View -> Html.Attribute
styleView view =
  case view of
    Styling.Menu ->
      class "autocomplete-menu-default"

    Styling.List ->
      class "autocomplete-list-default"

    Styling.Item ->
      class "autocomplete-item-default"

    Styling.SelectedItem ->
      class "autocomplete-selected-item-default"

    Styling.Input ->
      class "autocomplete-input-default"


main : Signal Html.Html
main =
  let
    config =
      Autocomplete.defaultConfig
        |> Autocomplete.setStyleViewFn styleView
  in
    StartApp.Simple.start
      { model = initWithConfig testData config
      , update = update
      , view = view
      }
