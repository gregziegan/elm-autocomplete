module Autocomplete.Styling (View(Menu, List, Item, SelectedItem, Input), defaultStyles) where

{-| Styling module for the Autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
This module includes functions to provide css class names for styling those
child views.

# Child Views
@docs View

# Defaults
@docs defaultStyles

-}

import Html exposing (Attribute)
import Html.Attributes exposing (style)


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


{-| Produces a style attribute for a View. Uses some pretty defaults.
-}
defaultStyles : View -> Attribute
defaultStyles view =
  case view of
    Menu ->
      menuStyle

    List ->
      listStyle

    Item ->
      itemStyle

    SelectedItem ->
      selectedItemStyle

    Input ->
      inputStyle



-- DEFAULTS


menuStyle : Attribute
menuStyle =
  style
    [ ( "position", "absolute" )
    , ( "left", "5px" )
    , ( "margin-top", "5px" )
    , ( "background", "white" )
    , ( "color", "black" )
    , ( "border", "1px solid #DDD" )
    , ( "border-radius", "3px" )
    , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
    , ( "min-width", "120px" )
    , ( "z-index", "11110" )
    ]


selectedItemStyle : Attribute
selectedItemStyle =
  style
    [ ( "background", "#3366FF" )
    , ( "color", "white" )
    , ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


listStyle : Attribute
listStyle =
  style
    [ ( "list-style", "none" )
    , ( "padding", "0" )
    , ( "margin", "auto" )
    , ( "max-height", "200px" )
    , ( "overflow-y", "auto" )
    ]


itemStyle : Attribute
itemStyle =
  style
    [ ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


inputStyle : Attribute
inputStyle =
  style
    [ ( "min-width", "120px" )
    , ( "color", "black" )
    , ( "position", "relative" )
    , ( "display", "block" )
    , ( "padding", "0.8em" )
    , ( "font-size", "12px" )
    ]
