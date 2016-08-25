module Autocomplete
    exposing
        ( State
        , KeySelected
        , MouseSelected
        , empty
        , reset
        , resetToFirstItem
        , Msg
        , UpdateConfig
        , updateConfig
        , update
        , view
        , viewWithSections
        , ViewConfig
        , ViewWithSectionsConfig
        , HtmlDetails
        , viewConfig
        , viewWithSectionsConfig
        , SectionConfig
        , SectionNode
        , sectionConfig
        , subscription
        )

{-| A customizable Autocomplete component.


# Definition
@docs State, KeySelected, MouseSelected, empty, reset, resetToFirstItem, ViewConfig, ViewWithSectionsConfig, HtmlDetails, viewConfig

# Update
@docs Msg, update, UpdateConfig, updateConfig, subscription

# View
@docs view, viewWithSections, SectionConfig, viewWithSectionsConfig, sectionConfig, SectionNode

-}

import Autocomplete.Autocomplete as Internal
import Html exposing (..)
import Html.App as Html
import Char exposing (KeyCode)


{-| The Autocomplete model.
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
type UpdateConfig msg data
    = UpdateConfig (Internal.UpdateConfig msg data)


{-| -}
update : UpdateConfig msg data -> Msg -> State -> List data -> Int -> ( State, Maybe msg )
update (UpdateConfig config) (Msg msg) (State state) data howManyToShow =
    let
        ( newState, maybeMsg ) =
            Internal.update config msg state data howManyToShow
    in
        ( State newState, maybeMsg )


{-| -}
updateConfig :
    { onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , toId : data -> String
    }
    -> UpdateConfig msg data
updateConfig config =
    UpdateConfig <| Internal.updateConfig config


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub Msg
subscription =
    Sub.map Msg Internal.subscription


{-| -}
view : ViewConfig data -> Int -> State -> List data -> Html Msg
view (ViewConfig config) howManyToShow (State state) data =
    Html.map Msg <| Internal.view config howManyToShow state data


{-| -}
viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State -> List sectionData -> Html Msg
viewWithSections (ViewWithSectionsConfig config) howManyToShow (State state) sections =
    Html.map Msg <| Internal.viewWithSections config howManyToShow state sections


{-| -}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| -}
type ViewConfig data
    = ViewConfig (Internal.ViewConfig data)


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


{-| -}
type ViewWithSectionsConfig data sectionData
    = ViewWithSectionsConfig (Internal.ViewWithSectionsConfig data sectionData)


{-| -}
viewWithSectionsConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig config =
    ViewWithSectionsConfig
        <| case config.section of
            SectionConfig section ->
                Internal.viewWithSectionsConfig { config | section = section }


{-| A section of the menu
-}
type SectionConfig data sectionData
    = SectionConfig (Internal.SectionConfig data sectionData)


{-| -}
type alias SectionNode msg =
    { nodeType : String
    , attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Define a section
-}
sectionConfig :
    { toId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }
    -> SectionConfig data sectionData
sectionConfig section =
    SectionConfig <| Internal.sectionConfig section
