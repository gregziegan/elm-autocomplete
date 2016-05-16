module Main exposing (..)

import Autocomplete.Config
import Autocomplete exposing (initWithConfig, update, view)
import Autocomplete.Styling as Styling
import Html.App as Html


testData : List String
testData =
  [ "elm"
  , "makes"
  , "coding"
  , "life"
  , "easy"
  ]


getClasses : Styling.View -> Styling.Classes
getClasses view =
  case view of
    Styling.Menu ->
      [ ( "autocomplete-menu", True ) ]

    Styling.List ->
      [ ( "autocomplete-list", True ) ]

    Styling.Item ->
      [ ( "autocomplete-item", True ) ]

    Styling.SelectedItem ->
      [ ( "autocomplete-selected-item", True ) ]

    Styling.Input ->
      [ ( "autocomplete-input", True ) ]


main : Program Never
main =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setClassesFn getClasses
  in
    Html.beginnerProgram
      { model = initWithConfig testData config
      , update = (\act model -> fst (update act model))
      , view = view
      }
