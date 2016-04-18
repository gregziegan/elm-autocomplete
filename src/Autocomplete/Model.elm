module Autocomplete.Model (Model, init, initWithConfig) where

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue)


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias Model =
  { value : InputValue
  , items : List Text
  , matches : List Text
  , selectedItemIndex : Index
  , showMenu : Bool
  , config : Config
  }


init : List String -> Model
init items =
  { value = ""
  , items = items
  , matches = items
  , selectedItemIndex = 0
  , showMenu = False
  , config = Config.defaultConfig
  }


initWithConfig : List String -> Config -> Model
initWithConfig items config =
  { value = ""
  , items = items
  , matches = items
  , selectedItemIndex = 0
  , showMenu = False
  , config = config
  }
