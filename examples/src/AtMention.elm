module AtMention exposing (..)

import Autocomplete.Config
import Autocomplete exposing (Autocomplete)
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.App exposing (map)

people : List String
people =
  [ "Ada Lovelace"
  , "Alan Turing"
  , "Grace Hopper"
  ]


type alias AtMention =
  { autocomplete : Autocomplete
  , value : String
  }


getClasses : Styling.View -> Styling.Classes
getClasses view =
  case view of
    Styling.Menu ->
      [ ( "mentionSuggestions", True ) ]

    Styling.List ->
      [ ("mentionList", True ) ]

    Styling.Item ->
      [ ( "mention", True ) ]

    Styling.SelectedItem ->
      [ ("mentionSelected", True ), ("mention", True ) ]

    Styling.Input ->
      []

createAutocomplete : Autocomplete
createAutocomplete =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.isValueControlled True
        |> Autocomplete.Config.setClassesFn getClasses
  in
    Autocomplete.initWithConfig people config
      |> Autocomplete.showMenu True


init : AtMention
init =
  { autocomplete = createAutocomplete
  , value = ""
  }


type Msg
  = NoOp
  | Autocomplete Autocomplete.Msg
  | SetValue String
  | ShowMenu Bool
  | NavigateMenu Autocomplete.MenuNavigation


type alias Completed =
  Bool


update : Msg -> AtMention -> ( AtMention, Completed )
update action model =
  case action of
    NoOp ->
      ( model, False )

    Autocomplete act ->
      let
        ( updatedAutocomplete, completed ) =
          Autocomplete.update act model.autocomplete
      in
        ( { model
            | autocomplete = updatedAutocomplete
            , value = Autocomplete.getCurrentValue updatedAutocomplete
          }
        , completed
        )

    SetValue value ->
      ( setValue value model, False )

    ShowMenu bool ->
      ( showMenu bool model, False )

    NavigateMenu navigation ->
      navigateMenu navigation model


navigateMenu : Autocomplete.MenuNavigation -> AtMention -> ( AtMention, Autocomplete.Status )
navigateMenu navigation model =
  let
    navMsg =
      Autocomplete.navigateMenu navigation model.autocomplete

    ( navigatedAuto, status ) =
      Autocomplete.update navMsg model.autocomplete

    updatedAutocomplete =
      if status.completed then
        Autocomplete.showMenu False navigatedAuto
      else
        navigatedAuto
  in
    ( { model
        | autocomplete = updatedAutocomplete
        , value = Autocomplete.getCurrentValue updatedAutocomplete
      }
    , status
    )


showMenu : Bool -> AtMention -> AtMention
showMenu bool model =
  { model | autocomplete = Autocomplete.showMenu bool model.autocomplete }


setValue : String -> AtMention -> AtMention
setValue value model =
  { model | value = value, autocomplete = Autocomplete.setValue value model.autocomplete }


getValue : AtMention -> String
getValue model =
  model.value


view : AtMention -> Html Msg
view model =
  map Autocomplete (Autocomplete.view model.autocomplete)
