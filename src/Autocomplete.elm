module Autocomplete (Autocomplete, GetItemsTask, init, initWithConfig, Action, update, view, getSelectedItemText) where

{-| A customizable Autocomplete component.

This Autocomplete has a dynamic list of items.
See the `Autocomplete.Simple` module for using a simple, static list of items.

The Autocomplete consists of a menu, a list, the list's many items, and an input.
All of these views are styleable via css classes.
See the `Autocomplete.Styling` module.

The currently selected item is preserved and styled with the aforementioned module.

This selection is modified by keyboard arrow input, mouse clicks, and API consumer defined keyCodes.

This Autocomplete calls a API consumer-defined function that returns a refreshed list
of items upon every input or selection change.

An example of plugging this into `StartApp`:
```
fetchMoreItems : String -> Task Effects.Never (List String)
fetchMoreItems url =
  Http.url url []
    |> Http.getString
    |> Task.toMaybe
    |> Task.map responseToItems


responseToItems : Maybe String -> List String
responseToItems maybeString =
  case maybeString of
    Just string ->
      String.lines string

    Nothing ->
      []


getItemsTask : String -> Int -> Task Effects.Never (List String)
getItemsTask value index =
  fetchMoreItems "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"


app =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setLoadingDisplay (img [ src "assets/loading.svg" ] [])
  in
    StartApp.start
      { init = Autocomplete.initWithConfig [] getItemsTask config
      , update = Autocomplete.update
      , view = Autocomplete.view
      , inputs = []
      }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
```

The above example can be found in `example/src/RemoteExample.elm`.

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
type Autocomplete
  = Autocomplete
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
  ( Autocomplete
      { value = ""
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
  ( Autocomplete
      { value = ""
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
update action (Autocomplete model) =
  case action of
    NoOp ->
      ( Autocomplete model, Effects.none )

    SetValue value ->
      updateInputValue value (Autocomplete model)

    UpdateItems items ->
      ( Autocomplete
          { model
            | items = items
            , matches =
                List.filter (\item -> (getConfig model.config).filterFn item model.value) model.items
                  |> List.sortWith (getConfig model.config).compareFn
            , showLoading = False
          }
      , Effects.none
      )

    Complete ->
      case getSelectedItem <| Autocomplete model of
        Just item ->
          ( Autocomplete { model | value = item, showMenu = False }, Effects.none )

        Nothing ->
          ( Autocomplete model, Effects.none )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min ((getConfig model.config).maxListSize - 1)
      in
        ( Autocomplete { model | selectedItemIndex = boundedNewIndex }, Effects.none )

    ShowMenu bool ->
      ( Autocomplete { model | showMenu = bool }, Effects.none )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Signal.Address Action -> Autocomplete -> Html
view address (Autocomplete model) =
  div
    [ onBlur address (ShowMenu False) ]
    [ viewInput address (Autocomplete model)
    , if not model.showMenu then
        div [] []
      else if model.showLoading then
        (getConfig model.config).loadingDisplay
      else if List.isEmpty model.matches then
        (getConfig model.config).noMatchesDisplay
      else
        viewMenu address (Autocomplete model)
    ]


viewInput : Signal.Address Action -> Autocomplete -> Html
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
      else if (List.member code (getConfig model.config).completionKeyCodes) then
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
      , (getConfig model.config).styleViewFn Styling.Input
      , autocomplete True
      ]
      []


viewItem : Signal.Address Action -> Autocomplete -> String -> Index -> Html
viewItem address (Autocomplete model) item index =
  li
    [ (getConfig model.config).styleViewFn Styling.Item
    , onMouseEnter address (ChangeSelection index)
    ]
    [ (getConfig model.config).itemHtmlFn item ]


viewSelectedItem : Signal.Address Action -> Autocomplete -> String -> Html
viewSelectedItem address (Autocomplete model) item =
  li
    [ (getConfig model.config).styleViewFn Styling.SelectedItem
    , onClick address Complete
    ]
    [ (getConfig model.config).itemHtmlFn item ]


viewMenu : Signal.Address Action -> Autocomplete -> Html
viewMenu address (Autocomplete model) =
  div
    [ (getConfig model.config).styleViewFn Styling.Menu
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
      [ (getConfig model.config).styleViewFn Styling.List
      ]
      (List.indexedMap getItemView model.matches
        |> List.take (getConfig model.config).maxListSize
      )



-- Effects


getMoreItems : String -> Autocomplete -> Effects Action
getMoreItems value (Autocomplete model) =
  model.getItemsTask value model.selectedItemIndex
    |> Task.map UpdateItems
    |> Effects.task



-- Helpers


updateInputValue : String -> Autocomplete -> ( Autocomplete, Effects Action )
updateInputValue value (Autocomplete model) =
  if value == "" then
    ( Autocomplete
        { model
          | value = value
          , matches =
              model.items
                |> List.sortWith (getConfig model.config).compareFn
          , selectedItemIndex = 0
        }
    , Effects.none
    )
  else
    let
      matches =
        List.filter (\item -> (getConfig model.config).filterFn item value) model.items
          |> List.sortWith (getConfig model.config).compareFn

      showLoading =
        if List.isEmpty matches then
          True
        else
          False
    in
      ( Autocomplete
          { model
            | value = value
            , matches = matches
            , showLoading = showLoading
            , selectedItemIndex = 0
          }
      , getMoreItems value (Autocomplete model)
      )


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem (Autocomplete model) =
  List.drop model.selectedItemIndex model.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> String
getSelectedItemText (Autocomplete model) =
  case (getSelectedItem (Autocomplete model)) of
    Just item ->
      item

    Nothing ->
      model.value


getConfig (Config.Config config) =
  config
