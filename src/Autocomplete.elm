module Autocomplete
    exposing
        ( State
        , KeySelected
        , MouseSelected
        , empty
        , reset
        , Msg
        , UpdateConfig
        , updateConfig
        , update
        , view
        , ViewConfig
        , HtmlDetails
        , viewConfig
        , subscription
        )

{-| A customizable Autocomplete component.


# Definition
@docs State, KeySelected, MouseSelected, empty, reset, ViewConfig, HtmlDetails, viewConfig

# Update
@docs Msg, update, UpdateConfig, updateConfig, subscription

# View
@docs view

-}

import Autocomplete.Autocomplete as Internal
import Html exposing (..)
import Char exposing (KeyCode)


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type alias State =
    Internal.State


{-| -}
type alias KeySelected =
    Internal.KeySelected


{-| -}
type alias MouseSelected =
    Internal.MouseSelected


{-| -}
empty : State
empty =
    Internal.empty


{-| -}
reset : State -> State
reset state =
    Internal.reset state



-- UPDATE


{-| -}
type alias Msg =
    Internal.Msg


{-| Configuration for updates
-}
type alias UpdateConfig msg =
    Internal.UpdateConfig msg


{-| -}
update : UpdateConfig msg -> Msg -> State -> ( State, Maybe msg )
update config msg state =
    Internal.update config msg state


{-| -}
updateConfig :
    { onChoose : String -> msg
    , onKeyChange : KeyCode -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe (String -> msg)
    , onMouseLeave : Maybe (String -> msg)
    , onMouseClick : Maybe (String -> msg)
    }
    -> UpdateConfig msg
updateConfig config =
    Internal.updateConfig config


subscription : List String -> Sub Msg
subscription ids =
    Internal.subscription ids


{-| -}
view : ViewConfig a -> Int -> State -> List a -> Html Msg
view config howManyToShow state data =
    Internal.view config howManyToShow state data



-- config howManyToShow state data inputValue


{-| -}
type alias HtmlDetails msg =
    Internal.HtmlDetails msg


{-| -}
type alias ViewConfig a =
    Internal.ViewConfig a


{-| -}
viewConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    , isChooseKey : KeyCode -> Bool
    }
    -> ViewConfig data
viewConfig =
    Internal.viewConfig
