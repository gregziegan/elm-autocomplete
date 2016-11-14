module Autocomplete.Internal exposing (..)

import Menu
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html
import String
import Json.Decode as Json
import Json.Encode as JE
import Task
import Dom


type alias Model =
    { menuState : Menu.State
    , query : String
    , selectedId : Maybe String
    , showMenu : Bool
    }


init : Model
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


update : Config item -> List item -> Int -> Msg -> Model -> ( Model, Cmd Msg )
update config items howManyToShow msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not << List.isEmpty <| config.filterItems newQuery items
            in
                { model | query = newQuery, showMenu = showMenu, selectedId = Nothing } ! []

        SetMenuState menuMsg ->
            let
                ( newState, maybeMsg ) =
                    Menu.update menuUpdateConfig menuMsg howManyToShow model.menuState (config.filterItems model.query items)

                newModel =
                    { model | menuState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        HandleEscape ->
            let
                validOptions =
                    not <| List.isEmpty (config.filterItems model.query items)

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu
                    else
                        { model | query = "" }
                            |> removeSelection
                            |> resetMenu

                escapedModel =
                    case model.selectedId of
                        Just id ->
                            if model.query == id then
                                model
                                    |> resetInput
                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
                escapedModel ! []

        Wrap toTop ->
            case model.selectedId of
                Just selectedId ->
                    update Reset model

                Nothing ->
                    if toTop then
                        { model
                            | menuState = Menu.resetToLastItem menuUpdateConfig (config.filterItems model.query items) howManyToShow model.menuState
                            , selectedId = List.head <| List.reverse <| List.take howManyToShow <| (config.filterItems model.query items)
                        }
                            ! []
                    else
                        { model
                            | menuState = Menu.resetToFirstItem menuUpdateConfig (config.filterItems model.query items) howManyToShow model.menuState
                            , selectedId = List.head <| List.take howManyToShow <| (config.filterItems model.query items)
                        }
                            ! []

        Reset ->
            { model | menuState = Menu.reset menuUpdateConfig model.menuState, selectedId = Nothing } ! []

        SelectItemKeyboard id ->
            let
                newModel =
                    setQuery config model id items
                        |> resetMenu
            in
                newModel ! []

        SelectItemMouse id ->
            let
                newModel =
                    setQuery config model id
                        |> resetMenu
            in
                ( newModel, Task.attempt (\_ -> NoOp) (Dom.focus "autocomplete-input") )

        PreviewItem id ->
            { model | selectedId = getItemAtId config items id } ! []

        OnFocus ->
            model ! []

        NoOp ->
            model ! []


resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection model =
    { model | selectedId = Nothing }


getItemAtId config items id =
    List.filter (\item -> (config.toId item) == id) items
        |> List.head


setQuery config model items id =
    { model
        | query =
            Maybe.map .name (getItemAtId config items id)
                |> Maybe.withDefault ""
        , selectedId = Maybe.map .id (getItemAtId config items id)
    }


resetMenu model =
    { model
        | menuState = Menu.empty
        , showMenu = False
    }


type alias Config item =
    { toId : item -> String
    , value : item -> String
    , filterItems : String -> List item -> List item
    }


menuUpdateConfig : Config item -> Menu.UpdateConfig Msg item
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


view : Config item -> List item -> Int -> Model -> Html Msg
view config items howManyToShow model =
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

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

        menu =
            if model.showMenu then
                [ viewMenu config items howManyToShow model ]
            else
                []

        query =
            Maybe.map config.value (getItemAtId config items id)
                |> Maybe.withDefault query

        activeDescendant attributes =
            case model.selectedId of
                Just person ->
                    (attribute "aria-activedescendant"
                        config.value
                        person
                    )
                        :: attributes

                Nothing ->
                    attributes
    in
        div []
            ([ input
                (activeDescendant
                    [ onInput SetQuery
                    , onFocus OnFocus
                    , onWithOptions "keydown" options dec
                    , value query
                    , class "autocomplete-input"
                    , autocomplete False
                    , attribute "aria-owns" "autocomplete-options"
                    , attribute "aria-expanded" <| String.toLower <| toString model.showMenu
                    , attribute "aria-haspopup" <| String.toLower <| toString model.showMenu
                    , attribute "role" "combobox"
                    , attribute "aria-autocomplete" "list"
                    ]
                )
                []
             ]
                ++ menu
            )


viewMenu : Config item -> List item -> Int -> Model -> Html Msg
viewMenu config items howManyToShow model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetMenuState (Menu.view viewConfig howManyToShow model.menuState (config.filterItems model.query items)) ]


viewConfig : Config item -> Menu.ViewConfig item
viewConfig config =
    let
        customizedLi keySelected mouseSelected item =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                ]
            , children = [ Html.text <| config.value item ]
            }
    in
        Menu.viewConfig
            { toId = config.toId
            , ul = [ class "autocomplete-list" ]
            , li = customizedLi
            }
