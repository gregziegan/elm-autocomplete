module Autocomplete.Internal exposing (..)

import Menu
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html
import String
import Json.Decode as Json
import Task
import Dom
import Char exposing (KeyCode)


type alias State =
    { menuState : Menu.State
    , query : String
    , selectedId : Maybe String
    , showMenu : Bool
    }


init : String -> Maybe String -> State
init query selectedId =
    { menuState = Menu.empty
    , query = query
    , selectedId = selectedId
    , showMenu = False
    }


type Msg
    = SetQuery String
    | SetMenuState Menu.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectItemKeyboard String
    | SelectItemMouse String
    | PreviewItem String
    | OnFocus
    | NoOp


update : Config msg item -> List item -> Int -> Msg -> State -> ( State, Cmd Msg, Maybe msg )
update config items howManyToShow msg state =
    let
        menuConfig =
            menuUpdateConfig config
    in
        case msg of
            SetQuery newQuery ->
                let
                    showMenu =
                        not << List.isEmpty <| config.filterItems newQuery items
                in
                    ( { state | query = newQuery, showMenu = showMenu, selectedId = Nothing }
                    , Cmd.none
                    , Nothing
                    )

            SetMenuState menuMsg ->
                let
                    ( newMenuState, maybeMsg ) =
                        Menu.update menuConfig menuMsg howManyToShow state.menuState (config.filterItems state.query items)

                    newState =
                        { state | menuState = newMenuState }
                in
                    case maybeMsg of
                        Nothing ->
                            ( newState, Cmd.none, Nothing )

                        Just updateMsg ->
                            update config items howManyToShow updateMsg newState

            HandleEscape ->
                let
                    validOptions =
                        not <| List.isEmpty (config.filterItems state.query items)

                    handleEscape =
                        if validOptions then
                            state
                                |> removeSelection
                                |> resetMenu
                        else
                            state
                                |> resetQuery
                                |> removeSelection
                                |> resetMenu

                    shouldResetInput : String -> String -> State
                    shouldResetInput query id =
                        if query == id then
                            resetInput state
                        else
                            state

                    escapedState =
                        Maybe.map (shouldResetInput state.query) state.selectedId
                            |> Maybe.withDefault handleEscape
                in
                    ( escapedState, Cmd.none, Nothing )

            Wrap toTop ->
                case state.selectedId of
                    Just selectedId ->
                        update config items howManyToShow Reset state

                    Nothing ->
                        if toTop then
                            ( { state
                                | menuState = Menu.resetToLastItem menuConfig (config.filterItems state.query items) howManyToShow state.menuState
                                , selectedId =
                                    config.filterItems state.query items
                                        |> List.take howManyToShow
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.map config.toId
                              }
                            , Cmd.none
                            , Nothing
                            )
                        else
                            ( { state
                                | menuState = Menu.resetToFirstItem menuConfig (config.filterItems state.query items) howManyToShow state.menuState
                                , selectedId =
                                    config.filterItems state.query items
                                        |> List.take howManyToShow
                                        |> List.head
                                        |> Maybe.map config.toId
                              }
                            , Cmd.none
                            , Nothing
                            )

            Reset ->
                ( { state | menuState = Menu.reset menuConfig state.menuState, selectedId = Nothing }
                , Cmd.none
                , Nothing
                )

            SelectItemKeyboard id ->
                let
                    newState =
                        setQuery config items id state
                            |> resetMenu
                in
                    ( newState
                    , Cmd.none
                    , Nothing
                    )

            SelectItemMouse id ->
                let
                    newState =
                        state
                            |> setQuery config items id
                            |> resetMenu
                in
                    ( newState
                    , Task.attempt (\_ -> NoOp) (Dom.focus "autocomplete-input")
                    , Nothing
                    )

            PreviewItem id ->
                ( { state | selectedId = Just id }
                , Cmd.none
                , Nothing
                )

            OnFocus ->
                ( state
                , Cmd.none
                , Nothing
                )

            NoOp ->
                ( state
                , Cmd.none
                , Nothing
                )


resetQuery : State -> State
resetQuery state =
    { state | query = "" }


resetInput : State -> State
resetInput state =
    state
        |> resetQuery
        |> removeSelection
        |> resetMenu


removeSelection : State -> State
removeSelection state =
    { state | selectedId = Nothing }


getItemAtId : (item -> String) -> List item -> String -> Maybe item
getItemAtId toId items id =
    List.filter (\item -> (toId item) == id) items
        |> List.head


setQuery :
    { a | toId : item -> String, toValue : item -> String }
    -> List item
    -> String
    -> State
    -> State
setQuery { toId, toValue } items id state =
    { state
        | query =
            Maybe.map toValue (getItemAtId toId items id)
                |> Maybe.withDefault ""
        , selectedId = Maybe.map toId (getItemAtId toId items id)
    }


resetMenu : State -> State
resetMenu state =
    { state
        | menuState = Menu.empty
        , showMenu = False
    }


type alias Config msg item =
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
    , li : Bool -> Bool -> item -> { attributes : List (Attribute Never), children : List (Html Never) }
    }


type alias ConfigWithSections msg item section =
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
    , li : Bool -> Bool -> item -> { attributes : List (Attribute Never), children : List (Html Never) }
    , sectionToId : section -> String
    , filterSections : String -> List section -> List section
    , getSectionItems : section -> List item
    , sections : List (Attribute Never)
    , section : section -> { nodeType : String, attributes : List (Attribute Never), children : List (Html Never) }
    }


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
    , li : Bool -> Bool -> item -> { attributes : List (Attribute Never), children : List (Html Never) }
    }
    -> Config msg item
config (config as c) =
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
    , li : Bool -> Bool -> item -> { attributes : List (Attribute Never), children : List (Html Never) }
    , sectionToId : section -> String
    , filterSections : String -> List section -> List section
    , getSectionItems : section -> List item
    , sections : List (Attribute Never)
    , section : section -> { nodeType : String, attributes : List (Attribute Never), children : List (Html Never) }
    }
    -> ConfigWithSections msg item section
configWithSections (config as c) =
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


menuUpdateConfig : Config msg item -> Menu.UpdateConfig Msg item
menuUpdateConfig config =
    Menu.updateConfig
        { toId = config.toId
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewItem maybeId
                else if code == 13 then
                    Maybe.map SelectItemKeyboard maybeId
                else
                    Just <| Reset
        , onTooLow = Just <| Wrap False
        , onTooHigh = Just <| Wrap True
        , onMouseEnter = \id -> Just <| PreviewItem id
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectItemMouse id
        , separateSelections = False
        }


fromResult : Result String a -> Json.Decoder a
fromResult result =
    case result of
        Ok val ->
            Json.succeed val

        Err reason ->
            Json.fail reason


autocompleteInput :
    { config | toId : item -> String, toValue : item -> String }
    -> List item
    -> State
    -> Html Msg
autocompleteInput ({ toId, toValue } as config) items ({ showMenu, query, selectedId } as state) =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else if code == 27 then
                        Ok HandleEscape
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen fromResult

        activeDescendantAttr itemValue =
            attribute "aria-activedescendant" itemValue

        addToAttrs attrs ariaAttr =
            ariaAttr :: attrs

        activeDescendant items selectedId attributes =
            selectedId
                |> Maybe.andThen (getItemAtId toId items)
                |> Maybe.map (activeDescendantAttr << toValue)
                |> Maybe.map (addToAttrs attributes)
                |> Maybe.withDefault attributes

        currentQuery items query selectedId =
            selectedId
                |> Maybe.andThen (getItemAtId toId items)
                |> Maybe.map toValue
                |> Maybe.withDefault query
    in
        input
            (activeDescendant
                items
                selectedId
                [ onInput SetQuery
                , onFocus OnFocus
                , onWithOptions "keydown" options dec
                , value <| currentQuery items query selectedId
                , class "autocomplete-input"
                , autocomplete False
                , attribute "aria-owns" "autocomplete-options"
                , attribute "aria-expanded" <| String.toLower <| toString showMenu
                , attribute "aria-haspopup" <| String.toLower <| toString showMenu
                , attribute "role" "combobox"
                , attribute "aria-autocomplete" "list"
                ]
            )
            []


view : Config msg item -> List item -> Int -> State -> Html Msg
view config items howManyToShow state =
    let
        input =
            autocompleteInput config items state

        menu =
            viewMenu config items howManyToShow state
    in
        div [ class "autocomplete" ]
            (if state.showMenu then
                [ input, menu ]
             else
                [ input ]
            )


viewMenu : Config msg item -> List item -> Int -> State -> Html Msg
viewMenu config items howManyToShow state =
    div [ class "autocomplete-menu" ]
        [ Html.map SetMenuState (Menu.view (viewConfig config) howManyToShow state.menuState (config.filterItems state.query items)) ]


viewWithSections : ConfigWithSections msg item section -> List section -> Int -> State -> Html Msg
viewWithSections config sections howManyToShow state =
    let
        items =
            List.concatMap config.getSectionItems sections

        input =
            autocompleteInput config items state

        menu =
            viewMenuWithSections config sections howManyToShow state
    in
        div [ class "autocomplete" ]
            (if state.showMenu then
                [ input, menu ]
             else
                [ input ]
            )


viewMenuWithSections : ConfigWithSections msg item section -> List section -> Int -> State -> Html Msg
viewMenuWithSections config sections howManyToShow state =
    div [ class "autocomplete-menu" ]
        [ Menu.viewWithSections (viewWithSectionsConfig config) howManyToShow state.menuState (config.filterSections state.query sections)
            |> Html.map SetMenuState
        ]


viewConfig : Config msg item -> Menu.ViewConfig item
viewConfig config =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                ]
            , children = [ Html.text <| config.toValue item ]
            }
    in
        Menu.viewConfig
            { toId = config.toId
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }


viewWithSectionsConfig : ConfigWithSections msg item section -> Menu.ViewWithSectionsConfig item section
viewWithSectionsConfig config =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected )
                    , ( "mouse-selected", mouseSelected )
                    ]
                ]
            , children = [ Html.text <| config.toValue item ]
            }
    in
        Menu.viewWithSectionsConfig
            { toId = config.toId
            , ul = [ class "autocomplete-list-with-sections" ]
            , li = customizedLi
            , section = sectionConfig config
            }


sectionConfig : ConfigWithSections msg item section -> Menu.SectionConfig item section
sectionConfig { sectionToId, getSectionItems, sections, section } =
    Menu.sectionConfig
        { toId = sectionToId
        , getData = getSectionItems
        , ul = sections
        , li = section
        }
