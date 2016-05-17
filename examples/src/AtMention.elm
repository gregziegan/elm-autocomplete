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
  = Autocomplete Autocomplete.Msg
  | SetValue String
  | ShowMenu Bool
  | NavigateMenu Autocomplete.MenuNavigation


update : Msg -> AtMention -> ( AtMention, Autocomplete.Status )
update msg model =
  case msg of
    Autocomplete autoMsg ->
      let
        ( updatedAutocomplete, status ) =
          Autocomplete.update autoMsg model.autocomplete
      in
        ( { model
            | autocomplete = updatedAutocomplete
            , value = Autocomplete.getCurrentValue updatedAutocomplete
          }
        , status
        )

    SetValue value ->
      let
        defaultStatus = Autocomplete.defaultStatus
      in
      ( setValue value model,  { defaultStatus | valueChanged = True } )

    ShowMenu bool ->
      ( showMenu bool model, Autocomplete.defaultStatus )

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
