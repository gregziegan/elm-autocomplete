module Autocomplete.Simple (Autocomplete, init, initWithConfig, Action, update, view, getSelectedItemText) where

{-| A customizable Autocomplete component.

This Autocomplete has a static list of items. See the Autocomplete module for maintaining a dynamic list of items.

The Autocomplete consists of a menu, a list, the list's many items, and an input.
All of these views are styleable via css classes.
See the Styling module.

The currently selected item is preserved and styled with the aforementioned module.

This selection is modified by keyboard arrow input, mouse clicks, and API consumer defined keyCodes.

Check out how easy it is to plug into `StartApp`:
```
main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = Autocomplete.init [ "elm", "makes", "coding", "life", "easy" ]
    , update = Autocomplete.update
    , view = Autocomplete.view
    }
```

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
type Autocomplete
  = Autocomplete
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
  Autocomplete
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
  Autocomplete
    { value = ""
    , items = items
    , matches = items
    , selectedItemIndex = 0
    , showMenu = False
    , config = config
    }


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
update action (Autocomplete model) =
  case action of
    NoOp ->
      Autocomplete model

    SetValue value ->
      if value == "" then
        Autocomplete
          { model
            | value = value
            , matches =
                model.items
                  |> List.sortWith model.config.compareFn
            , selectedItemIndex = 0
          }
      else
        Autocomplete
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
            Autocomplete { model | value = item, showMenu = False }

          Nothing ->
            Autocomplete model

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min (model.config.maxListSize - 1)
      in
        Autocomplete { model | selectedItemIndex = boundedNewIndex }

    ShowMenu bool ->
      Autocomplete { model | showMenu = bool }


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address (Autocomplete model) =
  div
    [ onBlur address (ShowMenu False) ]
    [ viewInput address (Autocomplete model)
    , if not model.showMenu then
        div [] []
      else if List.isEmpty model.matches then
        model.config.noMatchesDisplay
      else
        viewMenu address (Autocomplete model)
    ]


viewInput : Address Action -> Autocomplete -> Html
viewInput address (Autocomplete model) =
  let
    arrowUp =
      38

    arrowDown =
      40

    handleKeyDown code =
      if code == arrowUp then
        ChangeSelection (model.selectedItemIndex - 1)
      else if code == arrowDown then
        ChangeSelection (model.selectedItemIndex + 1)
      else if List.member code model.config.completionKeyCodes then
        Complete
      else
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
viewItem address (Autocomplete model) item index =
  li
    [ model.config.styleViewFn Styling.Item
    , onMouseEnter address (ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Signal.Address Action -> Autocomplete -> Text -> Html
viewSelectedItem address (Autocomplete model) item =
  li
    [ model.config.styleViewFn Styling.SelectedItem
    , onClick address Complete
    ]
    [ model.config.itemHtmlFn item ]


viewMenu : Signal.Address Action -> Autocomplete -> Html
viewMenu address (Autocomplete model) =
  div
    [ model.config.styleViewFn Styling.Menu
    ]
    [ viewList address (Autocomplete model) ]


viewList : Signal.Address Action -> Autocomplete -> Html
viewList address (Autocomplete model) =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem address (Autocomplete model) item
      else
        viewItem address (Autocomplete model) item index
  in
    ul
      [ model.config.styleViewFn Styling.List
      ]
      (List.indexedMap getItemView model.matches)



-- Helpers


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem (Autocomplete model) =
  List.drop model.selectedItemIndex model.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> Text
getSelectedItemText (Autocomplete model) =
  case (getSelectedItem (Autocomplete model)) of
    Just item ->
      item

    Nothing ->
      model.value
