module Main (..) where

import Autocomplete.Simple exposing (init, update, view)
import StartApp.Simple
import Html


testData : List Item
testData =
  [ initItem "0" "elm"
  , initItem "1" "makes"
  , initItem "2" "coding"
  , initItem "3" "life"
  , initItem "4" "easy"
  ]


initExampleClassListConfig : ClassListConfig
initExampleClassListConfig =
  { menu = [ ( "autocomplete-menu-default", True ) ]
  , item = [ ( "autocomplete-item-default", True ) ]
  , selectedItem = [ ( "autocomplete-selected-item-default", True ) ]
  , list = [ ( "autocomplete-list-default", True ) ]
  , input = [ ( "autocomplete-input-default", True ) ]
  }


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init testData 5 initExampleClassListConfig
    , update = update
    , view = view
    }
