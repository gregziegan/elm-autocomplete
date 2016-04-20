module Autocomplete.Simple (Autocomplete, IsComplete, init, initWithConfig, Action, update, view, getSelectedItemText, getCurrentValue, showMenu, setValue, isComplete) where

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
@docs Autocomplete, IsComplete

# Initialize
@docs init, initWithConfig

# Update
@docs Action, update

# Views
@docs view

# Helpers
@docs getSelectedItemText, getCurrentValue

# Controlling Behavior
@docs showMenu, setValue, isComplete

-}

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue)
import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Styling as Styling
import Autocomplete.Model exposing (Model)
import Autocomplete.View exposing (viewMenu)
import Autocomplete.Update as Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Signal exposing (..)


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


{-| Is the autocomplete completed?
-}
type alias IsComplete =
  Bool


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
update : Action -> Autocomplete -> ( Autocomplete, IsComplete )
update action (Autocomplete model) =
  case action of
    UpdateAutocomplete act ->
      let
        ( updatedModel, isComplete ) =
          Autocomplete.update act model
      in
        ( Autocomplete updatedModel, isComplete )

    SetValue value ->
      let
        ( updatedModel, isComplete ) =
          Autocomplete.update (Autocomplete.SetValue value) model
      in
        ( Autocomplete updatedModel, isComplete )


{-| The full Autocomplete view, with menu and input.
    Needs a Signal.Address and Autocomplete (typical of the Elm Architecture).
-}
view : Address Action -> Autocomplete -> Html
view address (Autocomplete model) =
  div
    [ onBlur address (UpdateAutocomplete (Autocomplete.ShowMenu False)) ]
    [ if model.config.isValueControlled then
        div [] []
      else
        viewInput address model
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
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.customDecoder
        keyCode
        (\k ->
          if List.member k (List.append [ 38, 40 ] model.config.completionKeyCodes) then
            Ok k
          else
            Err "not handling that key"
        )
      )

    handleKeyDown code =
      case code of
        38 ->
          UpdateAutocomplete
            <| Autocomplete.ChangeSelection (model.selectedItemIndex - 1)

        40 ->
          UpdateAutocomplete
            <| Autocomplete.ChangeSelection (model.selectedItemIndex + 1)

        _ ->
          UpdateAutocomplete Autocomplete.Complete
  in
    input
      [ type' "text"
      , on "input" targetValue (Signal.message address << SetValue)
      , onWithOptions
          "keydown"
          options
          dec
          (\k -> Signal.message address <| handleKeyDown k)
      , onFocus address (UpdateAutocomplete (Autocomplete.ShowMenu True))
      , value model.value
      , if model.config.useDefaultStyles then
          DefaultStyles.inputStyle
        else
          classList <| model.config.getClasses Styling.Input
      ]
      []


{-| Set whether the menu should be shown
-}
showMenu : Bool -> Autocomplete -> Autocomplete
showMenu bool auto =
  fst (update (UpdateAutocomplete (Autocomplete.ShowMenu True)) auto)


{-| Set current autocomplete value
-}
setValue : String -> Autocomplete -> Autocomplete
setValue value auto =
  fst (update (SetValue value) auto)


{-| Returns true if Autocomplete matches an item exactly
-}
isComplete : Autocomplete -> Bool
isComplete (Autocomplete model) =
  List.member model.value model.items



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
