module Autocomplete.Config exposing (Config, ItemHtmlFn, Text, InputValue, Index, Accessibility, Completed, ValueChanged, SelectionChanged, defaultConfig, hideMenuIfEmpty, isValueControlled, setClassesFn, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay, setAccessibilityProperties)

{-| Configuration module for the Autocomplete component.

# Definition
@docs Config, ItemHtmlFn, Text, InputValue, Index, Completed, ValueChanged, SelectionChanged, Accessibility

# Defaults
@docs defaultConfig

# Modifiers
@docs hideMenuIfEmpty, isValueControlled, setClassesFn, setCompletionKeyCodes, setItemHtml, setMaxListSize, setFilterFn, setCompareFn, setNoMatchesDisplay, setLoadingDisplay, setAccessibilityProperties


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
    , accessibility : Maybe Accessibility
    , hideMenuIfEmpty : Bool
    }


{-| Information needed for better screen reader accessibility.
    `owneeID` will differentiate multiple instances of the autocomplete.
-}
type alias Accessibility =
    { owneeID : String }


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


{-| Provide True to hide the autocomplete menu if the input field is empty.
    False to show the autocomplete menu whenever the input field has focus.
    The default config provides False.
-}
hideMenuIfEmpty : Bool -> Config msg -> Config msg
hideMenuIfEmpty bool config =
    { config | hideMenuIfEmpty = bool }


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


{-| Provide accessibility properties. Namely an owneeID for ariaOwneeID and to compute ariaActiveDescendantID
-}
setAccessibilityProperties : Accessibility -> Config msg -> Config msg
setAccessibilityProperties accessibility config =
    { config | accessibility = Just accessibility }



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
    , accessibility = Nothing
    , hideMenuIfEmpty = False
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
