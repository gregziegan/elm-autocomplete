module Autocomplete exposing (Autocomplete, Status, init, initWithConfig, Msg, update, view, getSelectedItem, getCurrentValue, showMenu, setValue, setItems, setLoading, MenuNavigation, navigateMenu, defaultStatus)

{-| A customizable Autocomplete component.

The Autocomplete consists of a menu, a list, the list's many items, and an input.
All of these views are styleable via css classes.
See the Styling module.

The currently selected item is preserved and styled with the aforementioned module.

This selection is modified by keyboard arrow input, mouse clicks, and API consumer defined keyCodes.

Check out how easy it is to plug into a simple program:
```
main =
  let
    updateAutocomplete msg autocomplete =
      let
        ( updatedAutocomplete, status ) = Autocomplete.update msg autocomplete
        -- status communicates extra information the parent on every update
        -- e.g. when the selection changes, the value changes, or the user has triggered a completion
      in
        updatedAutocomplete
  in
    Html.beginnerProgram
      { model = Autocomplete.init [ "elm", "makes", "coding", "life", "easy" ]
      , update = updateAutocomplete
      , view = Autocomplete.view
      }
```

# Definition
@docs Autocomplete, Status

# Initialize
@docs init, initWithConfig

# Update
@docs Msg, update

# Views
@docs view

# Helpers
@docs getSelectedItem, getCurrentValue

# Controlling Behavior

If you want the autocomplete to be completely controlled, with no `input` field, use the Config
module's `isValueControlled` function to designate that the API consumer will control the autocomplete.
This is useful for mentions and other autocomplete examples inside textareas, contenteditable, etc.

Defined below are functions to control:
  the autocomplete's menu navigation, set its value, items, and whether its menu should be shown.

@docs showMenu, setValue, setItems, setLoading, MenuNavigation, navigateMenu

# Defaults
@docs defaultStatus

-}

import Autocomplete.Autocomplete as Internal
import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue, Completed, ValueChanged, SelectionChanged)
import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Autocomplete =
    Internal.Autocomplete


type alias Model =
    Internal.Model


{-| Information for parent components about the update of the Autocomplete
-}
type alias Status =
    Internal.Status


{-| A description of a state change
-}
type alias Msg =
    Internal.Msg


{-| Creates an Autocomplete from a list of items with a default `String.startsWith` filter
-}
init : List String -> Autocomplete
init items =
    Internal.init items


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> Config.Config Msg -> Autocomplete
initWithConfig items config =
    Internal.initWithConfig items config


{-| The quintessential Elm Architecture reducer.
-}
update : Msg -> Autocomplete -> ( Autocomplete, Status )
update msg auto =
    Internal.update msg auto


{-| The full Autocomplete view, with menu and input.
-}
view : Autocomplete -> Html Msg
view auto =
    Internal.view auto



-- CONTROL FUNCTIONS


{-| Set whether the menu should be shown
-}
showMenu : Bool -> Autocomplete -> Autocomplete
showMenu bool auto =
    Internal.showMenu bool auto


{-| Set current autocomplete value
-}
setValue : String -> Autocomplete -> Autocomplete
setValue value auto =
    Internal.setValue value auto


{-| Sets the Autocomplete's list of items
-}
setItems : List String -> Autocomplete -> Autocomplete
setItems items auto =
    Internal.setItems items auto


{-| Sets whether the Autocomplete shows its loading display or not. Useful for remote updates.
-}
setLoading : Bool -> Autocomplete -> Autocomplete
setLoading bool auto =
    Internal.setLoading bool auto


{-| The possible actions to navigate the autocomplete menu
-}
type alias MenuNavigation =
    Internal.MenuNavigation


{-| When controlling the Autocomplete value, use this function
    to provide a message for updating the menu selection.
-}
navigateMenu : MenuNavigation -> Autocomplete -> Msg
navigateMenu navigation auto =
    Internal.navigateMenu navigation auto



-- HELPERS


{-| Get the text of the currently selected item
-}
getSelectedItem : Autocomplete -> Text
getSelectedItem auto =
    Internal.getSelectedItem auto


{-| Get the string currently entered by the user in the Autocomplete
-}
getCurrentValue : Autocomplete -> String
getCurrentValue auto =
    Internal.getCurrentValue auto



-- DEFAULTS


{-| A status record where everything is False
-}
defaultStatus : Status
defaultStatus =
    Internal.defaultStatus
