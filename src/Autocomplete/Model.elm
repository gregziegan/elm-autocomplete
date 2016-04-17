module Autocomplete.Model (Model) where

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
