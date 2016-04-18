module AtMention (..) where

import Autocomplete.Simple as Autocomplete exposing (Autocomplete)
import Autocomplete.Config


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
  , showMenu = True
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
      { model
        | value = value
        , showMenu = not (Autocomplete.isComplete model.autocomplete)
        , autocomplete = Autocomplete.showMenu (not <| Autocomplete.isComplete model.autocomplete) model.autocomplete
      }

    ShowMenu bool ->
      { model | showMenu = bool, autocomplete = Autocomplete.showMenu bool model.autocomplete }
