module Main (..) where

import Autocomplete.Config
import Autocomplete.Simple as Autocomplete exposing (initWithConfig, update, view)
import Autocomplete.Styling as Styling
import StartApp.Simple
import Html


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


main : Signal Html.Html
main =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setGetClasses getClasses
  in
    StartApp.Simple.start
      { model = initWithConfig testData config
      , update = update
      , view = view
      }
