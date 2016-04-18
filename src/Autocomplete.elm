module Autocomplete (Autocomplete, GetItemsTask, init, initWithConfig, Action, update, view, getSelectedItemText, getCurrentValue) where

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
@docs getSelectedItemText, getCurrentValue

-}

import Autocomplete.Config as Config exposing (Config, Index, Text, InputValue)
import Autocomplete.Model exposing (Model)
import Autocomplete.Update as Autocomplete
import Autocomplete.View exposing (viewMenu)
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
      { autocomplete : Model
      , getItemsTask : GetItemsTask
      , showLoading : Bool
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
      { autocomplete = Autocomplete.Model.init items
      , getItemsTask = getItemsTask
      , showLoading = False
      }
  , Effects.none
  )


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> GetItemsTask -> Config -> ( Autocomplete, Effects Action )
initWithConfig items getItemsTask config =
  ( Autocomplete
      { autocomplete = Autocomplete.Model.initWithConfig items config
      , getItemsTask = getItemsTask
      , showLoading = False
      }
  , Effects.none
  )


{-| A description of a state change
-}
type Action
  = UpdateAutocomplete Autocomplete.Action
  | SetValue String
  | UpdateItems (List String)


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> ( Autocomplete, Effects Action )
update action (Autocomplete model) =
  case action of
    UpdateAutocomplete act ->
      ( Autocomplete { model | autocomplete = Autocomplete.update act model.autocomplete }
      , Effects.none
      )

    SetValue value ->
      updateInputValue value (Autocomplete model)

    UpdateItems items ->
      ( Autocomplete
          { model
            | autocomplete = Autocomplete.update (Autocomplete.UpdateItems items) model.autocomplete
            , showLoading = False
          }
      , Effects.none
      )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Signal.Address Action -> Autocomplete -> Html
view address (Autocomplete model) =
  div
    [ onBlur (Signal.forwardTo address UpdateAutocomplete) (Autocomplete.ShowMenu False) ]
    [ viewInput address (Autocomplete model)
    , if not model.autocomplete.showMenu then
        div [] []
      else if model.showLoading then
        model.autocomplete.config.loadingDisplay
      else if List.isEmpty model.autocomplete.matches then
        model.autocomplete.config.noMatchesDisplay
      else
        viewMenu (Signal.forwardTo address UpdateAutocomplete) model.autocomplete
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
        UpdateAutocomplete (Autocomplete.ChangeSelection (model.autocomplete.selectedItemIndex - 1))
      else if code == arrowDown then
        UpdateAutocomplete (Autocomplete.ChangeSelection (model.autocomplete.selectedItemIndex + 1))
      else if (List.member code model.autocomplete.config.completionKeyCodes) then
        UpdateAutocomplete Autocomplete.Complete
      else
        UpdateAutocomplete Autocomplete.NoOp
  in
    input
      [ type' "text"
      , on "input" targetValue (Signal.message address << SetValue)
      , on "keydown" keyCode (\code -> Signal.message address (handleKeyDown code))
      , onFocus address (UpdateAutocomplete (Autocomplete.ShowMenu True))
      , value model.autocomplete.value
      , model.autocomplete.config.styleViewFn Styling.Input
      , autocomplete True
      ]
      []



-- Effects


getMoreItems : String -> Autocomplete -> Effects Action
getMoreItems value (Autocomplete model) =
  model.getItemsTask value model.autocomplete.selectedItemIndex
    |> Task.map UpdateItems
    |> Effects.task


updateInputValue : String -> Autocomplete -> ( Autocomplete, Effects Action )
updateInputValue value (Autocomplete model) =
  let
    updatedAutocomplete =
      Autocomplete.update (Autocomplete.SetValue value) model.autocomplete
  in
    if value == "" then
      ( Autocomplete
          { model
            | autocomplete = updatedAutocomplete
          }
      , Effects.none
      )
    else
      let
        showLoading =
          if List.isEmpty updatedAutocomplete.matches then
            True
          else
            False
      in
        ( Autocomplete
            { model
              | autocomplete = updatedAutocomplete
              , showLoading = showLoading
            }
        , getMoreItems value (Autocomplete model)
        )



-- HELPERS


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem (Autocomplete model) =
  List.drop model.autocomplete.selectedItemIndex model.autocomplete.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> Text
getSelectedItemText (Autocomplete model) =
  case getSelectedItem <| (Autocomplete model) of
    Just item ->
      item

    Nothing ->
      model.autocomplete.value


{-| Get the string currently entered by the user in the Autocomplete
-}
getCurrentValue : Autocomplete -> String
getCurrentValue (Autocomplete model) =
  model.autocomplete.value
