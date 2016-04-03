module Main (..) where

import Autocomplete.Simple exposing (init, update, view)
import StartApp.Simple
import Html


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init [ "elm", "makes", "coding", "life", "easy" ]
    , update = update
    , view = view
    }
