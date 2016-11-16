module Autocomplete exposing (..)

{-|

# View
@docs view

# Update
@docs update

# Configuration
@docs config

# State
@docs State, init, KeySelected, MouseSelected

# Definitions
@docs Msg, Config, HtmlDetails

**Note:** Section data can have any shape: your static configuration will
just tell the menu how to grab an ID for a section and its related data.

# View
@docs viewWithSections

# Configuration
@docs configWithSections

# Definitions
@docs ConfigWithSections
-}

import Autocomplete.Internal as Internal
import Html exposing (..)
import Html
import Char exposing (KeyCode)


{-|

-}
type State
    = State Internal.State


{-|

-}
init : String -> Maybe String -> State
init query selectedId =
    State <| Internal.init query selectedId


{-|

-}
type Msg
    = Msg Internal.Msg


{-|

-}
update : Config msg item -> List item -> Int -> Msg -> State -> ( State, Cmd Msg, Maybe msg )
update (Config config) items howManyToShow (Msg msg) (State state) =
    let
        ( newState, cmd, maybeMsg ) =
            Internal.update config items howManyToShow msg state
    in
        ( State newState, Cmd.map Msg cmd, maybeMsg )


{-|

-}
type Config msg item
    = Config (Internal.Config msg item)


{-|

-}
type ConfigWithSections msg item section
    = ConfigWithSections (Internal.ConfigWithSections msg item section)


{-|

-}
type alias KeySelected =
    Bool


{-|

-}
type alias MouseSelected =
    Bool


{-|

-}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }


{-|

-}
config :
    { toId : item -> String
    , toValue : item -> String
    , filterItems : String -> List item -> List item
    , onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , separateSelections : Bool
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> item -> HtmlDetails
    }
    -> Config msg item
config (config as c) =
    Config <|
        Internal.config
            { toId = c.toId
            , toValue = c.toValue
            , filterItems = c.filterItems
            , onKeyDown = c.onKeyDown
            , onTooLow = c.onTooLow
            , onTooHigh = c.onTooHigh
            , onMouseEnter = c.onMouseEnter
            , onMouseLeave = c.onMouseLeave
            , onMouseClick = c.onMouseClick
            , separateSelections = c.separateSelections
            , ul = c.ul
            , li = c.li
            }


{-|
  Config with sections
-}
configWithSections :
    { toId : item -> String
    , toValue : item -> String
    , filterItems : String -> List item -> List item
    , onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , separateSelections : Bool
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> item -> { attributes : List (Attribute Never), children : List (Html Never) }
    , sectionToId : section -> String
    , filterSections : String -> List section -> List section
    , getSectionItems : section -> List item
    , sections : List (Attribute Never)
    , section : section -> { nodeType : String, attributes : List (Attribute Never), children : List (Html Never) }
    }
    -> ConfigWithSections msg item section
configWithSections (config as c) =
    ConfigWithSections <|
        Internal.configWithSections
            { toId = c.toId
            , toValue = c.toValue
            , filterItems = c.filterItems
            , onKeyDown = c.onKeyDown
            , onTooLow = c.onTooLow
            , onTooHigh = c.onTooHigh
            , onMouseEnter = c.onMouseEnter
            , onMouseLeave = c.onMouseLeave
            , onMouseClick = c.onMouseClick
            , separateSelections = c.separateSelections
            , ul = c.ul
            , li = c.li
            , sectionToId = c.sectionToId
            , filterSections = c.filterSections
            , getSectionItems = c.getSectionItems
            , sections = c.sections
            , section = c.section
            }


{-|
view the thing
-}
view : Config msg item -> List item -> Int -> State -> Html Msg
view (Config config) items howManyToShow (State state) =
    Html.map Msg <| Internal.view config items howManyToShow state


{-|

-}
viewWithSections : ConfigWithSections msg item section -> List section -> Int -> State -> Html Msg
viewWithSections (ConfigWithSections config) sections howManyToShow (State state) =
    Html.map Msg <| Internal.viewWithSections config sections howManyToShow state
