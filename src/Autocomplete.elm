module Autocomplete
    exposing
        ( State
        , empty
        , reset
        , Msg
        , UpdateConfig
        , updateConfig
        , update
        , view
        , Config
        , HtmlDetails
        , config
        )

{-| A customizable Autocomplete component.


# Definition
@docs State, empty, reset, Config, HtmlDetails, config

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
    , onMouseEnter : Maybe msg
    , onMouseLeave : Maybe msg
    , onMouseClick : Maybe msg
    }
    -> UpdateConfig msg
updateConfig config =
    Internal.updateConfig config


{-| -}
view : Config a -> Int -> State -> List a -> Html Msg
view config howManyToShow state data =
    Internal.view config howManyToShow state data


{-| -}
type alias HtmlDetails msg =
    Internal.HtmlDetails msg


{-| -}
type alias Config a =
    Internal.Config a


{-| -}
config :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : Bool -> data -> HtmlDetails Never
    }
    -> Config data
config =
    Internal.config
