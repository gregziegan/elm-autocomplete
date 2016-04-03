module Autocomplete (Autocomplete, GetItemsTask, init, initWithConfig, Action, update, view, getSelectedItemText) where

{-| A customizable autocomplete component.

The autocomplete consists of a menu, a list, list items, and an input.
All of the aforementioned are styleable via css classes.

The currently selected item is preserved.

Selection is modified by keyboard input, mouse clicks,
and is also styled via css classes.

# Definition
@docs Autocomplete, GetItemsTask

# Initialize
@docs init, initWithConfig

# Update
@docs Action, update

# Views
@docs view

# Helpers
@docs getSelectedItemText

-}

import Autocomplete.Config as Config exposing (Config, Index, Text, InputValue)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects)
import Signal
import Task exposing (Task)
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
  , getItemsTask : GetItemsTask
  , showMenu : Bool
  , showLoading : Bool
  , config : Config
  }


{-| Consumer defined function that is used to retrieve more items. Called when either
the input's value or selection index is changed.
-}
type alias GetItemsTask =
  InputValue -> Index -> Task Effects.Never (List String)


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List String -> GetItemsTask -> ( Autocomplete, Effects Action )
init items getItemsTask =
  ( { value = ""
    , items = items
    , matches = items
    , selectedItemIndex = 0
    , getItemsTask = getItemsTask
    , showMenu = False
    , showLoading = False
    , config = Config.defaultConfig
    }
  , Effects.none
  )


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> GetItemsTask -> Config -> ( Autocomplete, Effects Action )
initWithConfig items getItemsTask config =
  ( { value = ""
    , items = items
    , matches = items
    , selectedItemIndex = 0
    , getItemsTask = getItemsTask
    , showMenu = False
    , showLoading = False
    , config = config
    }
  , Effects.none
  )


{-| A description of a state change
-}
type Action
  = NoOp
  | SetValue String
  | UpdateItems (List String)
  | Complete
  | ChangeSelection Int
  | ShowMenu Bool


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> ( Autocomplete, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    SetValue value ->
      updateInputValue value model

    UpdateItems items ->
      ( { model
          | items = items
          , matches =
              List.filter (\item -> model.config.filterFn item model.value) model.items
                |> List.sortWith model.config.compareFn
          , showLoading = False
        }
      , Effects.none
      )

    Complete ->
      case (getSelectedItem model) of
        Just item ->
          ( { model | value = item, showMenu = False }, Effects.none )

        Nothing ->
          ( model, Effects.none )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min (model.config.maxListSize - 1)
      in
        ( { model | selectedItemIndex = boundedNewIndex }, Effects.none )

    ShowMenu bool ->
      ( { model | showMenu = bool }, Effects.none )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Signal.Address Action -> Autocomplete -> Html
view address model =
  div
    [ onBlur address (ShowMenu False) ]
    [ viewInput address model
    , if not model.showMenu then
        div [] []
      else if model.showLoading then
        model.config.loadingDisplay
      else if List.isEmpty model.matches then
        model.config.noMatchesDisplay
      else
        viewMenu address model
    ]


viewInput : Signal.Address Action -> Autocomplete -> Html
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
      , autocomplete True
      ]
      []


viewItem : Signal.Address Action -> Autocomplete -> String -> Index -> Html
viewItem address model item index =
  li
    [ model.config.styleViewFn Styling.Item
    , onMouseEnter address (ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Signal.Address Action -> Autocomplete -> String -> Html
viewSelectedItem address model item =
  li
    [ model.config.styleViewFn Styling.SelectedItem
    , onClick address Complete
    ]
    [ model.config.itemHtmlFn item ]


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
        viewSelectedItem address model item
      else
        viewItem address model item index
  in
    ul
      [ model.config.styleViewFn Styling.List
      ]
      (List.indexedMap getItemView model.matches
        |> List.take model.config.maxListSize
      )



-- Effects


getMoreItems : String -> Autocomplete -> Effects Action
getMoreItems value model =
  model.getItemsTask value model.selectedItemIndex
    |> Task.map UpdateItems
    |> Effects.task



-- Helpers


updateInputValue : String -> Autocomplete -> ( Autocomplete, Effects Action )
updateInputValue value model =
  if value == "" then
    ( { model
        | value = value
        , matches =
            model.items
              |> List.sortWith model.config.compareFn
        , selectedItemIndex = 0
      }
    , Effects.none
    )
  else
    let
      matches =
        List.filter (\item -> model.config.filterFn item value) model.items
          |> List.sortWith model.config.compareFn

      showLoading =
        if List.isEmpty matches then
          True
        else
          False
    in
      ( { model
          | value = value
          , matches = matches
          , showLoading = showLoading
          , selectedItemIndex = 0
        }
      , getMoreItems value model
      )


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem model =
  List.drop model.selectedItemIndex model.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> String
getSelectedItemText model =
  case (getSelectedItem model) of
    Just item ->
      item

    Nothing ->
      model.value
