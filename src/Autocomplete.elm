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
        )

{-| A customizable Autocomplete component.


# Definition
@docs State, KeySelected, MouseSelected, empty, reset, ViewConfig, HtmlDetails, viewConfig

# Update
@docs Msg, update, UpdateConfig, updateConfig

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
    { onKeyDown : KeyCode -> Bool
    , onChoose : String -> msg
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


{-| -}
view : ViewConfig a -> Int -> State -> List a -> Html Msg
view config howManyToShow state data =
    Internal.view config howManyToShow state data


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
    }
    -> ViewConfig data
viewConfig =
    Internal.viewConfig
