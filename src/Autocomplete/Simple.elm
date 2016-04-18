module Autocomplete.Simple (Autocomplete, init, initWithConfig, Action, update, view, getSelectedItemText, getCurrentValue) where

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
@docs getSelectedItemText, getCurrentValue

-}

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Autocomplete.Styling as Styling
import Autocomplete.Model exposing (Model)
import Autocomplete.View exposing (viewMenu)
import Autocomplete.Update as Autocomplete


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type Autocomplete
  = Autocomplete Model


{-| A description of a state change
-}
type Action
  = UpdateAutocomplete Autocomplete.Action
  | SetValue String


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List String -> Autocomplete
init items =
  Autocomplete
    (Autocomplete.Model.init items)


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> Config.Config -> Autocomplete
initWithConfig items config =
  Autocomplete
    (Autocomplete.Model.initWithConfig items config)


{-| The quintessential Elm Architecture reducer.
-}
update : Action -> Autocomplete -> Autocomplete
update action (Autocomplete model) =
  case action of
    UpdateAutocomplete act ->
      Autocomplete (Autocomplete.update act model)

    SetValue value ->
      Autocomplete (Autocomplete.update (Autocomplete.SetValue value) model)


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address (Autocomplete model) =
  div
    [ onBlur address (UpdateAutocomplete (Autocomplete.ShowMenu False)) ]
    [ viewInput address model
    , if not model.showMenu then
        div [] []
      else if List.isEmpty model.matches then
        model.config.noMatchesDisplay
      else
        viewMenu (Signal.forwardTo address UpdateAutocomplete) model
    ]


viewInput : Address Action -> Model -> Html
viewInput address model =
  let
    arrowUp =
      38

    arrowDown =
      40

    handleKeyDown code =
      if code == arrowUp then
        UpdateAutocomplete (Autocomplete.ChangeSelection (model.selectedItemIndex - 1))
      else if code == arrowDown then
        UpdateAutocomplete (Autocomplete.ChangeSelection (model.selectedItemIndex + 1))
      else if List.member code model.config.completionKeyCodes then
        UpdateAutocomplete Autocomplete.Complete
      else
        UpdateAutocomplete Autocomplete.NoOp
  in
    input
      [ type' "text"
      , on "input" targetValue (Signal.message address << SetValue)
      , on "keydown" keyCode (\code -> Signal.message address (handleKeyDown code))
      , onFocus address (UpdateAutocomplete (Autocomplete.ShowMenu True))
      , value model.value
      , model.config.styleViewFn Styling.Input
      ]
      []



-- HELPERS


getSelectedItem : Autocomplete -> Maybe String
getSelectedItem (Autocomplete model) =
  List.drop model.selectedItemIndex model.matches
    |> List.head


{-| Get the text of the currently selected item
-}
getSelectedItemText : Autocomplete -> Text
getSelectedItemText (Autocomplete model) =
  case getSelectedItem <| (Autocomplete model) of
    Just item ->
      item

    Nothing ->
      model.value


{-| Get the string currently entered by the user in the Autocomplete
-}
getCurrentValue : Autocomplete -> String
getCurrentValue (Autocomplete model) =
  model.value
