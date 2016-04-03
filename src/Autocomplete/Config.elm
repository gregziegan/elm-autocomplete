module Autocomplete.Config (Config, ItemHtmlFn, Text, InputValue, Index, defaultConfig, setStyleViewFn, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay) where

{-| Configuration module for the Autocomplete component.

# Definition
@docs Config, ItemHtmlFn, Text, InputValue, Index

# Defaults
@docs defaultConfig

# Modifiers
@docs setStyleViewFn, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay


-}

import Html exposing (..)
import String
import Autocomplete.Styling as Styling


{-| The configuration record for an Autocomplete component.
-}
type alias Config =
  Model


type alias Model =
  { styleViewFn : Styling.View -> Attribute
  , itemHtmlFn : ItemHtmlFn
  , maxListSize : Int
  , filterFn : Text -> InputValue -> Bool
  , compareFn : Text -> Text -> Order
  , noMatchesDisplay : Html
  , loadingDisplay : Html
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


{-| Provide a function that produces an attribute to style a particular View
-}
setStyleViewFn : (Styling.View -> Attribute) -> Model -> Model
setStyleViewFn styleViewFn config =
  { config | styleViewFn = styleViewFn }


{-| Provide a custom HTML view for an Autocomplete item's text
-}
setItemHtml : ItemHtmlFn -> Model -> Model
setItemHtml itemHtmlFn config =
  { config | itemHtmlFn = itemHtmlFn }


{-| Provide a maximum list size for the Autocomplete menu
-}
setMaxListSize : Int -> Model -> Model
setMaxListSize maxListSize config =
  { config | maxListSize = maxListSize }


{-| Provide a custom filter function used to filter Autocomplete items.
-}
setFilterFn : (Text -> InputValue -> Bool) -> Model -> Model
setFilterFn filterFn config =
  { config | filterFn = filterFn }


{-| Provide a custom comparison function to order the Autocomplete matches.
-}
setCompareFn : (Text -> Text -> Order) -> Model -> Model
setCompareFn compareFn config =
  { config | compareFn = compareFn }


{-| Provide a custom HTML display for the case that nothing matches.
-}
setNoMatchesDisplay : Html -> Model -> Model
setNoMatchesDisplay noMatchesDisplay config =
  { config | noMatchesDisplay = noMatchesDisplay }


{-| Provide a custom loading display for the case when more items are being fetched
-}
setLoadingDisplay : Html -> Model -> Model
setLoadingDisplay loadingDisplay config =
  { config | loadingDisplay = loadingDisplay }



-- DEFAULTS


{-| A simple Autocomplete configuration
-}
defaultConfig : Model
defaultConfig =
  { styleViewFn = Styling.defaultStyles
  , itemHtmlFn = (\item -> text item)
  , maxListSize = 5
  , filterFn = (\item value -> String.startsWith value item)
  , compareFn = normalComparison
  , noMatchesDisplay = p [] [ text "No Matches" ]
  , loadingDisplay = p [] [ text "..." ]
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
