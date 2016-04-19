module Main (..) where

import Autocomplete.Simple exposing (Autocomplete, init, update, view)
import StartApp.Simple
import Html


simpleUpdate : Autocomplete.Simple.Action -> Autocomplete -> Autocomplete
simpleUpdate action autocomplete =
  fst (update action autocomplete)


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init [ "elm", "makes", "coding", "life", "easy" ]
    , update = simpleUpdate
    , view = view
    }
