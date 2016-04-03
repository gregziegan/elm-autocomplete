module Autocomplete.Config (..) where

{-| Configuration module for the Autocomplete component.

# Defaults
@docs defaultConfig

# Modifiers
@docs setStyleViewFn, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay


-}

import Html exposing (..)
import String
import Autocomplete.Styling as Styling


type alias Config =
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


{-| Provide a function that produces an attribute to style a particular View
-}
setStyleViewFn : (Styling.View -> Attribute) -> Config -> Config
setStyleViewFn styleViewFn config =
  { config | styleViewFn = styleViewFn }


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
