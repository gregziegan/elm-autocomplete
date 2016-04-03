module Autocomplete.Simple (Autocomplete, init, initWithConfig, Action, update, view, getSelectedItemText) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete

# Initialize
@docs init, initWithConfig

# Update
@docs Action, update

# Views
@docs view

# Helpers
@docs getSelectedItemText

-}

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Autocomplete.Styling as Styling


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Autocomplete =
  Model


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Model =
  { value : InputValue
  , items : List Text
  , matches : List Text
  , selectedItemIndex : Index
  , showMenu : Bool
  , config : Config
  }


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List String -> Autocomplete
init items =
  { value = ""
  , items = items
  , matches = items
  , selectedItemIndex = 0
  , showMenu = False
  , config = Config.defaultConfig
  }


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> Config.Config -> Autocomplete
initWithConfig items config =
  let
    model =
      init items
  in
    { model | config = config }


{-| A description of a state change
-}
type Action
  = NoOp
  | SetValue String
  | Complete
  | ChangeSelection Int
  | ShowMenu Bool


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> Autocomplete
update action model =
  case action of
    NoOp ->
      model

    SetValue value ->
      if value == "" then
        { model
          | value = value
          , matches =
              model.items
                |> List.sortWith model.config.compareFn
          , selectedItemIndex = 0
        }
      else
        { model
          | value = value
          , matches =
              List.filter (\item -> model.config.filterFn item value) model.items
                |> List.sortWith model.config.compareFn
          , selectedItemIndex = 0
        }

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


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address model =
  div
    [ onBlur address (ShowMenu False) ]
    [ viewInput address model
    , if not model.showMenu then
        div [] []
      else if List.isEmpty model.matches then
        model.config.noMatchesDisplay
      else
        viewMenu address model
    ]


viewInput : Address Action -> Autocomplete -> Html
viewInput address model =
  let
    handleKeyDown code =
      case code of
        38 ->
          ChangeSelection (model.selectedItemIndex - 1)

        40 ->
          ChangeSelection (model.selectedItemIndex + 1)

        9 ->
          Complete

        _ ->
          NoOp
  in
    input
      [ type' "text"
      , on "input" targetValue (Signal.message address << SetValue)
      , on "keydown" keyCode (\code -> Signal.message address (handleKeyDown code))
      , onFocus address (ShowMenu True)
      , value model.value
      , model.config.styleViewFn Styling.Input
      ]
      []


viewItem : Signal.Address Action -> Autocomplete -> Text -> Index -> Html
viewItem address model item index =
  li
    [ model.config.styleViewFn Styling.Item
    , onMouseEnter address (ChangeSelection index)
    ]
    [ text item ]


viewSelectedItem : Autocomplete -> Text -> Html
viewSelectedItem model item =
  li
    [ model.config.styleViewFn Styling.SelectedItem
    ]
    [ text item ]


viewMenu : Signal.Address Action -> Autocomplete -> Html
viewMenu address model =
  div
    [ model.config.styleViewFn Styling.Menu
    ]
    [ viewList address model ]


viewList : Signal.Address Action -> Autocomplete -> Html
viewList address model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem model item
      else
        viewItem address model item index
  in
    ul
      [ model.config.styleViewFn Styling.List
      ]
      (List.indexedMap getItemView model.matches)



-- Helpers


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem model =
  List.drop model.selectedItemIndex model.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> Text
getSelectedItemText model =
  case (getSelectedItem model) of
    Just item ->
      item

    Nothing ->
      model.value
