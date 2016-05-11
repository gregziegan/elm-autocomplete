module Autocomplete exposing (Autocomplete, GetItemsTask, init, initWithConfig, Msg, update, view, getSelectedItemText, getCurrentValue, showMenu, setValue, isComplete, MenuNavigation(Previous, Next, Select), navigateMenu)

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
fetchMoreItems : String -> Task Cmd.Never (List String)
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


getItemsTask : String -> Int -> Task Cmd.Never (List String)
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


port tasks =
  app.tasks
```

The above example can be found in `example/src/RemoteExample.elm`.

# Definition
@docs Autocomplete, GetItemsTask

# Initialize
@docs init, initWithConfig

# Update
@docs Msg, update

# Views
@docs view

# Helpers
@docs getSelectedItemText, getCurrentValue

# Controlling Behavior
@docs showMenu, setValue, isComplete, MenuNavigation, navigateMenu

-}

import Autocomplete.Config as Config exposing (Config, Index, Text, InputValue, Completed)
import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Model as Model exposing (Model)
import Autocomplete.Update as Update
import Autocomplete.View exposing (viewMenu)
import Html exposing (..)
import Html.App exposing (map)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
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
  InputValue -> Index -> Task String (List String)


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List String -> GetItemsTask -> ( Autocomplete, Cmd Msg )
init items getItemsTask =
  ( Autocomplete
      { autocomplete = Model.init items
      , getItemsTask = getItemsTask
      , showLoading = False
      }
  , Cmd.none
  )


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> GetItemsTask -> Config Model.Msg -> ( Autocomplete, Cmd a )
initWithConfig items getItemsTask config =
  ( Autocomplete
      { autocomplete = Model.initWithConfig items config
      , getItemsTask = getItemsTask
      , showLoading = False
      }
  , Cmd.none
  )


{-| A description of a state change
-}
type Msg
  = UpdateAutocomplete Model.Msg
  | SetValue String
  | UpdateItemsSuccess (List String)
  | UpdateItemsFail String


{-| The quintessential Elm Architecture reducer.
-}
update : Msg -> Autocomplete -> ( Autocomplete, Cmd Msg, Completed )
update action (Autocomplete model) =
  case action of
    UpdateAutocomplete act ->
      let
        ( updatedModel, completed ) =
          Update.update act model.autocomplete
        updatedAutocomplete =
          Autocomplete { model | autocomplete = updatedModel }
      in
        if completed && not model.autocomplete.config.isValueControlled then
           ( showMenu False updatedAutocomplete, Cmd.none, completed )
        else
          ( updatedAutocomplete, Cmd.none, completed )

    SetValue value ->
      let
          ( auto, effects, completed ) =
            updateInputValue value (Autocomplete model)
      in
          if not model.autocomplete.config.isValueControlled then
             ( showMenu True auto, effects, completed )
          else
            ( auto, effects, completed )


    UpdateItemsSuccess items ->
      let
        ( updatedModel, completed ) =
          Update.update (Model.UpdateItems items) model.autocomplete
      in
        ( Autocomplete
            { model
              | autocomplete = updatedModel
              , showLoading = False
            }
        , Cmd.none
        , completed
        )

    UpdateItemsFail  errMsg ->
      ( Autocomplete model, Cmd.none, False )


{-| The full Autocomplete view, with menu and input.
-}
view : Autocomplete -> Html Msg
view  (Autocomplete model) =
  div
    [ onBlur (UpdateAutocomplete (Model.ShowMenu False)) ]
    [ viewInput  (Autocomplete model)
    , if not model.autocomplete.showMenu then
        div [] []
      else if model.showLoading then
        map UpdateAutocomplete model.autocomplete.config.loadingDisplay
      else if List.isEmpty model.autocomplete.matches then
        map UpdateAutocomplete  model.autocomplete.config.noMatchesDisplay
      else
        map  UpdateAutocomplete (viewMenu model.autocomplete)
    ]


viewInput : Autocomplete -> Html Msg
viewInput  (Autocomplete model) =
  let
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.customDecoder
        keyCode
        (\code ->
          if code == 38 then
              Ok
                <| UpdateAutocomplete
                <| Model.ChangeSelection (model.autocomplete.selectedItemIndex - 1)
          else if code == 40 then
            Ok
                <| UpdateAutocomplete
                <| Model.ChangeSelection (model.autocomplete.selectedItemIndex + 1)
          else if List.member code model.autocomplete.config.completionKeyCodes then
              Ok (UpdateAutocomplete Model.Complete)
          else
            Err "not handling that key"
        )
      )
  in
    input
      [ type' "text"
      , onInput SetValue
      , onWithOptions "keydown" options dec
      , onFocus  (UpdateAutocomplete (Model.ShowMenu True))
      , value model.autocomplete.value
      , if model.autocomplete.config.useDefaultStyles then
          style DefaultStyles.inputStyles
        else
          classList <| model.autocomplete.config.getClasses Styling.Input
      ]
      []



-- Cmd


getMoreItems : String -> Autocomplete -> Cmd Msg
getMoreItems value (Autocomplete model) =
  model.getItemsTask value model.autocomplete.selectedItemIndex
    |> Task.perform UpdateItemsFail  UpdateItemsSuccess


updateInputValue : String -> Autocomplete -> ( Autocomplete, Cmd Msg, Completed )
updateInputValue value (Autocomplete model) =
  let
    ( updatedModel, completed ) =
      Update.update (Model.SetValue value) model.autocomplete
  in
    if value == "" then
      ( Autocomplete
          { model
            | autocomplete = updatedModel
          }
      , Cmd.none
      , completed
      )
    else
      let
        showLoading =
          if List.isEmpty updatedModel.matches then
            True
          else
            False
      in
        ( Autocomplete
            { model
              | autocomplete = updatedModel
              , showLoading = showLoading
            }
        , getMoreItems value (Autocomplete model)
        , completed
        )

-- CONTROL FUNCTIONS

{-| Set whether the menu should be shown
-}
showMenu : Bool -> Autocomplete -> Autocomplete
showMenu bool auto =
  let
    (auto, effects, completed) =
      update (UpdateAutocomplete (Model.ShowMenu bool)) auto
  in
    auto


{-| Set current autocomplete value
-}
setValue : String -> Autocomplete -> Autocomplete
setValue value auto =
  let
    (auto, effects, completed) =
      update (SetValue value) auto
  in
    auto


{-| Returns true if Autocomplete matches an item exactly
-}
isComplete : Autocomplete -> Bool
isComplete (Autocomplete model) =
  List.member model.autocomplete.value model.autocomplete.items

{-| The possible actions to navigate the autocomplete menu
-}
type MenuNavigation
  = Previous
  | Next
  | Select


{-| When controlling the Autocomplete value, use this function
    to provide an action for updating the menu selection.
-}
navigateMenu : MenuNavigation -> Autocomplete -> Msg
navigateMenu navigation (Autocomplete model) =
  case navigation of
    Previous ->
      UpdateAutocomplete
        <| Model.ChangeSelection (model.autocomplete.selectedItemIndex - 1)

    Next ->
      UpdateAutocomplete
        <| Model.ChangeSelection (model.autocomplete.selectedItemIndex + 1)

    Select ->
      UpdateAutocomplete Model.Complete


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
