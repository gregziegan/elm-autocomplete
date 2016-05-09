module Autocomplete.Config (Config, ItemHtmlFn, Text, InputValue, Index, Completed, defaultConfig, isValueControlled, setGetClasses, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay) where

{-| Configuration module for the Autocomplete component.

# Definition
@docs Config, ItemHtmlFn, Text, InputValue, Index, Completed

# Defaults
@docs defaultConfig

# Modifiers
@docs isValueControlled, setGetClasses, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay


-}

import Html exposing (..)
import String
import Autocomplete.Styling as Styling
import Char exposing (KeyCode)


{-| The configuration record for an Autocomplete component.
-}
type alias Config =
  Model


type alias Model =
  { getClasses : Styling.View -> Styling.Classes
  , useDefaultStyles : Bool
  , completionKeyCodes : List KeyCode
  , itemHtmlFn : ItemHtmlFn
  , maxListSize : Int
  , filterFn : Text -> InputValue -> Bool
  , compareFn : Text -> Text -> Order
  , noMatchesDisplay : Html
  , loadingDisplay : Html
  , isValueControlled : Bool
  }


{-| Given the text of an item, produce some HTML
-}
type alias ItemHtmlFn =
  Text -> Html


{-| The text of an item
-}
type alias Text =
  String


{-| The value of the input
-}
type alias InputValue =
  String


{-| Positive integer index of selected item in list
-}
type alias Index =
  Int


{-| True if an update completed the autocomplete
-}
type alias Completed =
  Bool


{-| Provide True to control the autocomplete value,
    False to let the component control the value via a stylable `input` field.

    The default config provides False.
-}
isValueControlled : Bool -> Config -> Config
isValueControlled bool config =
  { config | isValueControlled = bool }


{-| Provide a function that produces an list of classes to style a particular View
-}
setGetClasses : (Styling.View -> Styling.Classes) -> Config -> Config
setGetClasses getClasses config =
  { config | getClasses = getClasses, useDefaultStyles = False }


{-| Provide keycodes for autocompletion. By default, completion happens on tab press.
-}
setCompletionKeyCodes : List KeyCode -> Config -> Config
setCompletionKeyCodes keycodes config =
  { config | completionKeyCodes = keycodes }


{-| Provide a custom HTML view for an Autocomplete item's text
-}
setItemHtml : ItemHtmlFn -> Config -> Config
setItemHtml itemHtmlFn config =
  { config | itemHtmlFn = itemHtmlFn }


{-| Provide a maximum list size for the Autocomplete menu
-}
setMaxListSize : Int -> Config -> Config
setMaxListSize maxListSize config =
  { config | maxListSize = maxListSize }


{-| Provide a custom filter function used to filter Autocomplete items.
-}
setFilterFn : (Text -> InputValue -> Bool) -> Config -> Config
setFilterFn filterFn config =
  { config | filterFn = filterFn }


{-| Provide a custom comparison function to order the Autocomplete matches.
-}
setCompareFn : (Text -> Text -> Order) -> Config -> Config
setCompareFn compareFn config =
  { config | compareFn = compareFn }


{-| Provide a custom HTML display for the case that nothing matches.
-}
setNoMatchesDisplay : Html -> Config -> Config
setNoMatchesDisplay noMatchesDisplay config =
  { config | noMatchesDisplay = noMatchesDisplay }


{-| Provide a custom loading display for the case when more items are being fetched
-}
setLoadingDisplay : Html -> Config -> Config
setLoadingDisplay loadingDisplay config =
  { config | loadingDisplay = loadingDisplay }



-- DEFAULTS


{-| A simple Autocomplete configuration
-}
defaultConfig : Config
defaultConfig =
  { getClasses = (\view -> [])
  , useDefaultStyles = True
  , completionKeyCodes =
      [ 9 ]
      -- defaults to tab
  , itemHtmlFn = (\item -> text item)
  , maxListSize = 5
  , filterFn = (\item value -> String.startsWith value item)
  , compareFn = normalComparison
  , noMatchesDisplay = p [] [ text "No Matches" ]
  , loadingDisplay = p [] [ text "..." ]
  , isValueControlled = False
  }


normalComparison : String -> String -> Order
normalComparison item1 item2 =
  case compare item1 item2 of
    LT ->
      LT

    EQ ->
      EQ

    GT ->
      GT
