module Autocomplete
    exposing
        ( State
        , KeySelected
        , MouseSelected
        , empty
        , reset
        , resetToFirstItem
        , Msg(..)
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
@docs State, KeySelected, MouseSelected, empty, reset, resetToFirstItem, ViewConfig, HtmlDetails, viewConfig

# Update
@docs Msg, update, UpdateConfig, updateConfig, subscription

# View
@docs view

-}

import Autocomplete.Autocomplete as Internal
import Html exposing (..)
import Html.App as Html
import Char exposing (KeyCode)


{-| The Autocomplete model.
    It assumes filtering is based upon strings.
-}
type State
    = State Internal.State


{-| -}
type alias KeySelected =
    Bool


{-| -}
type alias MouseSelected =
    Bool


{-| -}
empty : State
empty =
    State Internal.empty


{-| -}
reset : State -> State
reset (State state) =
    State <| Internal.reset state


{-| -}
resetToFirstItem : List data -> (data -> String) -> State -> State
resetToFirstItem data toId (State state) =
    State <| Internal.resetToFirstItem data toId state



-- UPDATE


{-| -}
type Msg
    = Msg Internal.Msg


{-| Configuration for updates
-}
type UpdateConfig msg
    = UpdateConfig (Internal.UpdateConfig msg)


{-| -}
update : UpdateConfig msg -> Msg -> State -> ( State, Maybe msg )
update (UpdateConfig config) (Msg msg) (State state) =
    let
        ( newState, maybeMsg ) =
            Internal.update config msg state
    in
        ( State newState, maybeMsg )



-- Internal.update config (Internal msg) state


{-| -}
updateConfig :
    { onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe (String -> msg)
    , onMouseLeave : Maybe (String -> msg)
    , onMouseClick : Maybe (String -> msg)
    }
    -> UpdateConfig msg
updateConfig config =
    UpdateConfig <| Internal.updateConfig config


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : List String -> Sub Msg
subscription ids =
    Sub.map Msg (Internal.subscription ids)


{-| -}
view : ViewConfig a -> Int -> State -> List a -> Html Msg
view (ViewConfig config) howManyToShow (State state) data =
    Html.map Msg <| Internal.view config howManyToShow state data



-- config howManyToShow state data inputValue


{-| -}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| -}
type ViewConfig a
    = ViewConfig (Internal.ViewConfig a)


{-| -}
viewConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    }
    -> ViewConfig data
viewConfig config =
    ViewConfig <| Internal.viewConfig config
