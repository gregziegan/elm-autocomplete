module Autocomplete.Config exposing (Config, ItemHtmlFn, Text, InputValue, Index, Completed, ValueChanged, SelectionChanged, defaultConfig, isValueControlled, setClassesFn, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay)

{-| Configuration module for the Autocomplete component.

# Definition
@docs Config, ItemHtmlFn, Text, InputValue, Index, Completed, ValueChanged, SelectionChanged

# Defaults
@docs defaultConfig

# Modifiers
@docs isValueControlled, setClassesFn, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay


-}

import Html exposing (..)
import String
import Autocomplete.Styling as Styling
import Char exposing (KeyCode)


{-| The configuration record for an Autocomplete component.
-}
type alias Config msg =
  Model msg


type alias Model msg =
  { getClasses : Styling.View -> Styling.Classes
  , useDefaultStyles : Bool
  , completionKeyCodes : List KeyCode
  , itemHtmlFn : ItemHtmlFn msg
  , maxListSize : Int
  , filterFn : Text -> InputValue -> Bool
  , compareFn : Text -> Text -> Order
  , noMatchesDisplay : Html msg
  , loadingDisplay : Html msg
  , isValueControlled : Bool
  }


{-| Given the text of an item, produce some HTML
-}
type alias ItemHtmlFn msg =
  Text -> Html msg


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

{-| True if an update changed the autocomplete's value
-}
type alias ValueChanged =
  Bool

{-| True if an update changed the autocomplete's selection
-}
type alias SelectionChanged =
  Bool


{-| Provide True to control the autocomplete value,
    False to let the component control the value via a stylable `input` field.

    The default config provides False.
-}
isValueControlled : Bool -> Config msg -> Config msg
isValueControlled bool config =
  { config | isValueControlled = bool }


{-| Provide a function that produces an list of classes to style a particular View
-}
setClassesFn : (Styling.View -> Styling.Classes) -> Config msg -> Config msg
setClassesFn getClasses config =
  { config | getClasses = getClasses, useDefaultStyles = False }


{-| Provide keycodes for autocompletion. By default, completion happens on tab press.
-}
setCompletionKeyCodes : List KeyCode -> Config msg -> Config msg
setCompletionKeyCodes keycodes config =
  { config | completionKeyCodes = keycodes }


{-| Provide a custom HTML view for an Autocomplete item's text
-}
setItemHtml : ItemHtmlFn msg -> Config msg -> Config msg
setItemHtml itemHtmlFn config =
  { config | itemHtmlFn = itemHtmlFn }


{-| Provide a maximum list size for the Autocomplete menu
-}
setMaxListSize : Int -> Config msg -> Config msg
setMaxListSize maxListSize config =
  { config | maxListSize = maxListSize }


{-| Provide a custom filter function used to filter Autocomplete items.
-}
setFilterFn : (Text -> InputValue -> Bool) -> Config msg -> Config msg
setFilterFn filterFn config =
  { config | filterFn = filterFn }


{-| Provide a custom comparison function to order the Autocomplete matches.
-}
setCompareFn : (Text -> Text -> Order) -> Config msg -> Config msg
setCompareFn compareFn config =
  { config | compareFn = compareFn }


{-| Provide a custom HTML display for the case that nothing matches.
-}
setNoMatchesDisplay : Html msg -> Config msg -> Config msg
setNoMatchesDisplay noMatchesDisplay config =
  { config | noMatchesDisplay = noMatchesDisplay }


{-| Provide a custom loading display for the case when more items are being fetched
-}
setLoadingDisplay : Html msg -> Config msg -> Config msg
setLoadingDisplay loadingDisplay config =
  { config | loadingDisplay = loadingDisplay }



-- DEFAULTS


{-| A simple Autocomplete configuration
-}
defaultConfig : Config msg
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
