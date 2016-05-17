module Autocomplete exposing (Autocomplete, Status, init, initWithConfig, Msg, update, view, getSelectedItem, getCurrentValue, showMenu, setValue, isComplete, setItems, setLoading, MenuNavigation(Previous, Next, Select), navigateMenu, defaultStatus)

{-| A customizable Autocomplete component.

The Autocomplete consists of a menu, a list, the list's many items, and an input.
All of these views are styleable via css classes.
See the Styling module.

The currently selected item is preserved and styled with the aforementioned module.

This selection is modified by keyboard arrow input, mouse clicks, and API consumer defined keyCodes.

Check out how easy it is to plug into a simple program:
```
main =
  Html.beginnerProgram
    { model = Autocomplete.init [ "elm", "makes", "coding", "life", "easy" ]
    , update = Autocomplete.update
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
@docs showMenu, setValue, isComplete, setItems, setLoading, MenuNavigation, navigateMenu

# Defaults
@docs defaultStatus

-}

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue, Completed, ValueChanged, SelectionChanged)
import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type Autocomplete
  = Autocomplete Model

type alias Model =
  { value : InputValue
  , items : List Text
  , matches : List Text
  , selectedItemIndex : Index
  , showMenu : Bool
  , isLoading : Bool
  , config : Config Msg
  }

{-| Information for parent components about the update of the Autocomplete -}
type alias Status =
  { completed : Completed
  , valueChanged : ValueChanged
  , selectionChanged : SelectionChanged
  }

{-| A description of a state change
-}
type Msg
  = Complete
  | ChangeSelection Int
  | ShowMenu Bool
  | UpdateItems (List String)
  | SetValue String
  | SetLoading Bool


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
    , isLoading = False
    }


{-| Creates an Autocomplete with a custom configuration
-}
initWithConfig : List String -> Config.Config Msg -> Autocomplete
initWithConfig items config =
  Autocomplete
    { value = ""
    , items = items
    , matches = items
    , selectedItemIndex = 0
    , showMenu = False
    , isLoading = False
    , config = config
    }


{-| The quintessential Elm Architecture reducer.
-}
update : Msg -> Autocomplete-> ( Autocomplete, Status )
update msg auto =
  updateAutocomplete msg auto
    |> toggleMenu

updateAutocomplete : Msg -> Autocomplete -> ( Autocomplete, Status )
updateAutocomplete msg (Autocomplete model) =
  updateModel msg model
    |> makeOpaque

updateModel : Msg -> Model -> ( Model, Status )
updateModel msg model =
  case msg of
    Complete ->
      let
        selectedItem =
          List.drop model.selectedItemIndex model.matches
            |> List.head
      in
        case selectedItem of
          Just item ->
            ( { model | value = item }, { defaultStatus | completed = True, valueChanged = True } )

          Nothing ->
            ( model, { defaultStatus | completed = True } )

    ChangeSelection newIndex ->
      let
        boundedNewIndex =
          Basics.max newIndex 0
            |> Basics.min ((List.length model.matches) - 1)
            |> Basics.min (model.config.maxListSize - 1)
      in
        ( { model | selectedItemIndex = boundedNewIndex }, { defaultStatus | selectionChanged = True } )

    ShowMenu bool ->
      ( { model | showMenu = bool }, defaultStatus )

    UpdateItems items ->
      ( { model
          | items = items
          , matches =
              List.filter (\item -> model.config.filterFn item model.value) model.items
                |> List.sortWith model.config.compareFn
        }
      , defaultStatus
      )

    SetValue value ->
      if value == "" then
        ( { model
            | value = value
            , matches =
                model.items
                  |> List.sortWith model.config.compareFn
            , selectedItemIndex = 0
          }
        , { defaultStatus | valueChanged = True }
        )
      else
        ( { model
            | value = value
            , matches =
                List.filter (\item -> model.config.filterFn item value) model.items
                  |> List.sortWith model.config.compareFn
            , selectedItemIndex = 0
          }
        , { defaultStatus | valueChanged = True }
        )

    SetLoading bool ->
      ( { model | isLoading = bool }, defaultStatus )

toggleMenu : ( Autocomplete, Status ) -> (Autocomplete, Status )
toggleMenu ( Autocomplete model, status ) =
  if model.config.isValueControlled then
    ( Autocomplete model, status )
  else if status.completed then
     ( showMenu False (Autocomplete model), status )
  else
     ( showMenu True (Autocomplete model), status )


makeOpaque : ( Model, Status ) -> ( Autocomplete, Status)
makeOpaque (model, status)=
  ( Autocomplete model, status )

{-| The full Autocomplete view, with menu and input.
-}
view : Autocomplete -> Html Msg
view  (Autocomplete model) =
  div
    [ onBlur  (ShowMenu False) ]
    [ if model.config.isValueControlled then
        div [] []
      else
        viewInput  model
    , if not model.showMenu then
        div [] []
      else if model.isLoading then
        model.config.loadingDisplay
      else if List.isEmpty model.matches then
        model.config.noMatchesDisplay
      else
        viewMenu model
    ]


viewInput : Model -> Html Msg
viewInput  model =
  let
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.customDecoder
        keyCode
        (\code ->
            if code == 38 then
              Ok (navigateMenu Previous (Autocomplete model))
            else if code == 40 then
              Ok (navigateMenu Next (Autocomplete model))
            else if List.member code model.config.completionKeyCodes then
              Ok (navigateMenu Select (Autocomplete model))
            else
              Err "not handling that key"
        )
      )
  in
    input
      [ type' "text"
      , onInput SetValue
      , onWithOptions "keydown" options dec
      , onFocus (ShowMenu True)
      , value model.value
      , if model.config.useDefaultStyles then
          style DefaultStyles.inputStyles
        else
          classList <| model.config.getClasses Styling.Input
      ]
      []


viewItem : Model -> Text -> Index -> Html Msg
viewItem  model item index =
  li
    [ if model.config.useDefaultStyles then
        style DefaultStyles.itemStyles
      else
        classList <| model.config.getClasses Styling.Item
    , onMouseEnter (ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Model -> Text -> Html Msg
viewSelectedItem  model item =
  li
    [ if model.config.useDefaultStyles then
        style DefaultStyles.selectedItemStyles
      else
        classList <| model.config.getClasses Styling.SelectedItem
    , onClick Complete
    ]
    [ model.config.itemHtmlFn item ]


viewMenu : Model -> Html Msg
viewMenu model =
  div
    [ if model.config.useDefaultStyles then
        style DefaultStyles.menuStyles
      else
        classList <| model.config.getClasses Styling.Menu
    ]
    [ viewList  model ]


viewList : Model -> Html Msg
viewList  model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem model item
      else
        viewItem model item index
  in
    ul
      [ if model.config.useDefaultStyles then
          style DefaultStyles.listStyles
        else
          classList <| model.config.getClasses Styling.List
      ]
      (List.indexedMap getItemView model.matches)


-- CONTROL FUNCTIONS

{-| Set whether the menu should be shown
-}
showMenu : Bool -> Autocomplete -> Autocomplete
showMenu bool auto =
  fst (updateAutocomplete (ShowMenu bool) auto)


{-| Set current autocomplete value
-}
setValue : String -> Autocomplete -> Autocomplete
setValue value auto =
  fst (updateAutocomplete (SetValue value) auto)


{-| Returns true if Autocomplete matches an item exactly
-}
isComplete : Autocomplete -> Bool
isComplete (Autocomplete model) =
  List.member model.value model.items


{-| Sets the Autocomplete's list of items -}
setItems : List String -> Autocomplete -> Autocomplete
setItems items auto =
  fst (updateAutocomplete (UpdateItems items) auto)


{-| Sets whether the Autocomplete shows its loading display or not. Useful for remote updates. -}
setLoading : Bool -> Autocomplete -> Autocomplete
setLoading bool auto =
  fst (update (SetLoading bool) auto)


{-| The possible actions to navigate the autocomplete menu
-}
type MenuNavigation
  = Previous
  | Next
  | Select


{-| When controlling the Autocomplete value, use this function
    to provide a message for updating the menu selection.
-}
navigateMenu : MenuNavigation -> Autocomplete -> Msg
navigateMenu navigation (Autocomplete model) =
  case navigation of
    Previous ->
      ChangeSelection (model.selectedItemIndex - 1)

    Next ->
      ChangeSelection (model.selectedItemIndex + 1)

    Select ->
      Complete


-- HELPERS

{-| Get the text of the currently selected item
-}
getSelectedItem : Autocomplete -> Text
getSelectedItem (Autocomplete model) =
  let
    maybeSelectedItem = List.drop model.selectedItemIndex model.matches
      |> List.head
  in
    case maybeSelectedItem of
      Just item ->
        item

      Nothing ->
        model.value


{-| Get the string currently entered by the user in the Autocomplete
-}
getCurrentValue : Autocomplete -> String
getCurrentValue (Autocomplete model) =
  model.value


-- DEFAULTS

{-| A status record where everything is False
-}
defaultStatus : Status
defaultStatus =
  { completed = False
  , valueChanged = False
  , selectionChanged = False
  }
