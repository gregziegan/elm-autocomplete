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
  = Autocomplete Autocomplete.Action
  | SetValue String
  | ShowMenu Bool


update : Action -> AtMention -> AtMention
update action model =
  case action of
    Autocomplete act ->
      { model | autocomplete = Autocomplete.update act model.autocomplete }

    SetValue value ->
      setValue value model

    ShowMenu bool ->
      showMenu bool model


showMenu : Bool -> AtMention -> AtMention
showMenu bool model =
  { model | showMenu = bool, autocomplete = Autocomplete.showMenu bool model.autocomplete }


setValue : String -> AtMention -> AtMention
setValue value model =
  { model
    | value = value
    , showMenu = not (Autocomplete.isComplete model.autocomplete)
    , autocomplete = Autocomplete.showMenu (not <| (Autocomplete.isComplete model.autocomplete))) model.autocomplete
  }


getValue : AtMention -> String
getValue model =
  model.value


view : Signal.Address Action -> AtMention -> Html
view address model =
  div
    []
    [ Autocomplete.view (Signal.forwardTo address Autocomplete) model.autocomplete ]
