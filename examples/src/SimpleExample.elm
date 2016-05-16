module Main exposing (..)

import Autocomplete exposing (Autocomplete, init, update, view)
import Html.App as Html


simpleUpdate : Autocomplete.Msg -> Autocomplete -> Autocomplete
simpleUpdate action autocomplete =
  fst (update action autocomplete)


main : Program Never
main =
  Html.beginnerProgram
    { model = init [ "elm", "makes", "coding", "life", "easy" ]
    , update = simpleUpdate
    , view = view
    }
