module Main (..) where

import Autocomplete.Simple as Autocomplete exposing (getSelectedItemText)
import Autocomplete.Styling as Styling
import StartApp.Simple
import Html exposing (..)
import Html.Attributes exposing (style, class)
import String


styleView : Styling.View -> Html.Attribute
styleView view =
  case view of
    Styling.Menu ->
      class "autocomplete-menu-default"

    Styling.List ->
      class "autocomplete-list-default"

    Styling.Item ->
      class "autocomplete-item-default"

    Styling.SelectedItem ->
      class "autocomplete-selected-item-default"

    Styling.Input ->
      class "autocomplete-input-default"


type alias Model =
  { autocompleteRemaining : String
  , autocomplete : Autocomplete.Model
  }


init : Model
init =
  let
    config =
      Autocomplete.defaultConfig
        |> Autocomplete.setStyleViewFn styleView
  in
    { autocompleteRemaining = ""
    , autocomplete = Autocomplete.initWithConfig [ "elm", "makes", "coding", "life", "easy" ] config
    }


type Action
  = Autocomplete Autocomplete.Action


update : Action -> Model -> Model
update action model =
  case action of
    Autocomplete act ->
      let
        updatedAutocomplete =
          Autocomplete.update act model.autocomplete

        preview =
          getSelectedItemText updatedAutocomplete
      in
        { model
          | autocompleteRemaining =
              preview
                |> String.slice (String.length updatedAutocomplete.value) (String.length preview)
          , autocomplete = updatedAutocomplete
        }


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ( "position", "relative" ), ( "font-family", "monospace" ), ( "font-size", "12px" ) ] ]
    [ span
        [ style [ ( "position", "absolute" ), ( "left", "3px" ), ( "top", "3px" ) ] ]
        [ span [ style [ ( "visibility", "none" ) ] ] [ text model.autocomplete.value ]
        , span [ style [ ( "color", "gray" ) ] ] [ text model.autocompleteRemaining ]
        ]
    , Autocomplete.view (Signal.forwardTo address Autocomplete) model.autocomplete
    ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
