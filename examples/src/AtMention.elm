module AtMention (..) where

import Autocomplete.Simple as Autocomplete exposing (Autocomplete)
import Autocomplete.Config
import Html exposing (..)


people : List String
people =
  [ "Ada Lovelace"
  , "Alan Turing"
  , "Grace Hopper"
  ]


type alias AtMention =
  { autocomplete : Autocomplete
  , value : String
  , showMenu : Bool
  }


createAutocomplete : Autocomplete
createAutocomplete =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.isValueControlled True
  in
    Autocomplete.initWithConfig people config


init : AtMention
init =
  { autocomplete = createAutocomplete
  , value = ""
  , showMenu = False
  }


type Action
  = NoOp
  | Autocomplete Autocomplete.Action
  | SetValue String
  | ShowMenu Bool
  | NavigateMenu Autocomplete.MenuNavigation


type alias Completed =
  Bool


update : Action -> AtMention -> ( AtMention, Completed )
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


navigateMenu : Autocomplete.MenuNavigation -> AtMention -> ( AtMention, Completed )
navigateMenu navigation model =
  let
    navAction =
      Autocomplete.navigateMenu navigation model.autocomplete

    ( updatedAutocomplete, completed ) =
      Autocomplete.update navAction model.autocomplete
  in
    ( { model
        | autocomplete = updatedAutocomplete
        , showMenu = not completed
        , value = Autocomplete.getCurrentValue updatedAutocomplete
      }
    , completed
    )


showMenu : Bool -> AtMention -> AtMention
showMenu bool model =
  { model | showMenu = bool, autocomplete = Autocomplete.showMenu bool model.autocomplete }


setValue : String -> AtMention -> AtMention
setValue value model =
  { model | value = value, autocomplete = Autocomplete.setValue value model.autocomplete }


getValue : AtMention -> String
getValue model =
  model.value


view : Signal.Address Action -> AtMention -> Html
view address model =
  div
    []
    [ Autocomplete.view (Signal.forwardTo address Autocomplete) model.autocomplete ]