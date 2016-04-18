module Autocomplete.Styling (View(Menu, List, Item, SelectedItem, Input), Classes) where

{-| Styling module for the Autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
This module includes functions to provide css class names for styling those
child views.

Styling is easy as:
```
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
```

# Child Views
@docs View

# Definition
@docs Classes

-}


{-| A list of class names and their associated status (added/removed) as a boolean value.
-}
type alias Classes =
  List ( String, Bool )


{-| The stylable views of the Autocomplete component.
-}
type View
  = Menu
  | List
  | Item
  | SelectedItem
  | Input
