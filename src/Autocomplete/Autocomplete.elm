module Autocomplete.Autocomplete exposing (Autocomplete(..), Model, Status, Msg(..), MenuNavigation(..), init, initWithConfig, update, updateModel, view, navigateMenu, showMenu, setValue, setItems, setLoading, getSelectedItem, getCurrentValue, defaultStatus)

import Autocomplete.Config as Config exposing (Config, Text, Index, InputValue, Completed, ValueChanged, SelectionChanged)
import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String


type Autocomplete
    = Autocomplete Model


type alias Model =
    { value : InputValue
    , items : List Text
    , matches : List Text
    , selectedItemIndex : Index
    , showMenu : Bool
    , isLoading : Bool
    , config : Config Msg
    }


type alias Status =
    { completed : Completed
    , valueChanged : ValueChanged
    , selectionChanged : SelectionChanged
    }


init : List String -> Autocomplete
init items =
    Autocomplete
        { value = ""
        , items = items
        , matches = items
        , selectedItemIndex = 0
        , showMenu = False
        , config = Config.defaultConfig
        , isLoading = False
        }


initWithConfig : List String -> Config.Config Msg -> Autocomplete
initWithConfig items config =
    Autocomplete
        { value = ""
        , items = items
        , matches = items
        , selectedItemIndex = 0
        , showMenu = False
        , isLoading = False
        , config = config
        }


type Msg
    = Complete
    | ChangeSelection Int
    | ShowMenu Bool
    | UpdateItems (List String)
    | SetValue String
    | SetLoading Bool


update : Msg -> Autocomplete -> ( Autocomplete, Status )
update msg auto =
    case msg of
        ShowMenu bool ->
            updateAutocomplete msg auto

        _ ->
            updateAutocomplete msg auto
                |> toggleMenu


updateAutocomplete : Msg -> Autocomplete -> ( Autocomplete, Status )
updateAutocomplete msg (Autocomplete model) =
    updateModel msg model
        |> makeOpaque


updateModel : Msg -> Model -> ( Model, Status )
updateModel msg model =
    case msg of
        Complete ->
            let
                selectedItem =
                    List.drop model.selectedItemIndex model.matches
                        |> List.head
            in
                case selectedItem of
                    Just item ->
                        ( { model | value = item }, { defaultStatus | completed = True, valueChanged = True } )

                    Nothing ->
                        ( model, { defaultStatus | completed = True } )

        ChangeSelection newIndex ->
            let
                boundedNewIndex =
                    Basics.max newIndex 0
                        |> Basics.min ((List.length model.matches) - 1)
                        |> Basics.min (model.config.maxListSize - 1)
            in
                ( { model | selectedItemIndex = boundedNewIndex }, { defaultStatus | selectionChanged = True } )

        ShowMenu bool ->
            let
                shouldShowMenu =
                    if model.config.hideMenuIfEmpty && (String.isEmpty model.value) then
                        False
                    else
                        bool
            in
                ( { model | showMenu = shouldShowMenu }, defaultStatus )

        UpdateItems items ->
            ( { model
                | items = items
                , matches =
                    List.filter (\item -> model.config.filterFn item model.value) items
                        |> List.sortWith model.config.compareFn
              }
            , defaultStatus
            )

        SetValue value ->
            if value == "" then
                ( { model
                    | value = value
                    , matches =
                        model.items
                            |> List.sortWith model.config.compareFn
                    , selectedItemIndex = 0
                  }
                , { defaultStatus | valueChanged = True }
                )
            else
                ( { model
                    | value = value
                    , matches =
                        List.filter (\item -> model.config.filterFn item value) model.items
                            |> List.sortWith model.config.compareFn
                    , selectedItemIndex = 0
                  }
                , { defaultStatus | valueChanged = True }
                )

        SetLoading bool ->
            ( { model | isLoading = bool }, defaultStatus )


toggleMenu : ( Autocomplete, Status ) -> ( Autocomplete, Status )
toggleMenu ( Autocomplete model, status ) =
    if model.config.isValueControlled then
        ( Autocomplete model, status )
    else if status.completed then
        ( showMenu False (Autocomplete model), status )
    else
        ( showMenu True (Autocomplete model), status )


makeOpaque : ( Model, Status ) -> ( Autocomplete, Status )
makeOpaque ( model, status ) =
    ( Autocomplete model, status )


view : Autocomplete -> Html Msg
view (Autocomplete model) =
    div [ onBlur (ShowMenu False) ]
        [ if model.config.isValueControlled then
            div [] []
          else
            viewInput model
        , if not model.showMenu then
            div [] []
          else if model.isLoading then
            model.config.loadingDisplay
          else if List.isEmpty model.matches then
            model.config.noMatchesDisplay
          else
            viewMenu model
        ]


viewInput : Model -> Html Msg
viewInput model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        getStrBool bool =
            if bool then
                "true"
            else
                "false"

        dec =
            (Json.customDecoder keyCode
                (\code ->
                    if code == 38 then
                        Ok (navigateMenu Previous (Autocomplete model))
                    else if code == 40 then
                        Ok (navigateMenu Next (Autocomplete model))
                    else if code == 27 then
                        Ok (ShowMenu False)
                    else if List.member code model.config.completionKeyCodes then
                        Ok (navigateMenu Select (Autocomplete model))
                    else
                        Err "not handling that key"
                )
            )

        accessibilityAttributes =
            case model.config.accessibility of
                Just aria ->
                    let
                        descendantID =
                            aria.owneeID ++ "-" ++ (toString model.selectedItemIndex)
                    in
                        [ attribute "ariaActiveDescendantID" descendantID
                        , attribute "ariaOwneeID" aria.owneeID
                        ]

                Nothing ->
                    [ attribute "ariaActiveDescendantID" ""
                    , attribute "ariaOwneeID" ""
                    ]
    in
        input
            (List.append
                [ type' "text"
                , onInput SetValue
                , onWithOptions "keydown" options dec
                , onFocus (ShowMenu True)
                , onBlur (ShowMenu False)
                , value model.value
                , if model.config.useDefaultStyles then
                    style DefaultStyles.inputStyles
                  else
                    classList <| model.config.getClasses Styling.Input
                , attribute "role" "combobox"
                , attribute "ariaAutoComplete" "list"
                , attribute "ariaHasPopup" (getStrBool model.showMenu)
                , attribute "ariaExpanded" (getStrBool model.showMenu)
                ]
                accessibilityAttributes
            )
            []


viewItem : Model -> Text -> Index -> Html Msg
viewItem model item index =
    li
        [ if model.config.useDefaultStyles then
            style DefaultStyles.itemStyles
          else
            classList <| model.config.getClasses Styling.Item
        , onMouseEnter (ChangeSelection index)
        ]
        [ model.config.itemHtmlFn item ]


viewSelectedItem : Model -> Text -> Html Msg
viewSelectedItem model item =
    li
        [ if model.config.useDefaultStyles then
            style DefaultStyles.selectedItemStyles
          else
            classList <| model.config.getClasses Styling.SelectedItem
        , onMouseDown Complete
        ]
        [ model.config.itemHtmlFn item ]


viewMenu : Model -> Html Msg
viewMenu model =
    div
        [ if model.config.useDefaultStyles then
            style DefaultStyles.menuStyles
          else
            classList <| model.config.getClasses Styling.Menu
        ]
        [ viewList model ]


viewList : Model -> Html Msg
viewList model =
    let
        getItemView index item =
            if index == model.selectedItemIndex then
                viewSelectedItem model item
            else
                viewItem model item index

        constrainedMatches =
            List.take model.config.maxListSize model.matches
    in
        ul
            [ if model.config.useDefaultStyles then
                style DefaultStyles.listStyles
              else
                classList <| model.config.getClasses Styling.List
            ]
            (List.indexedMap getItemView constrainedMatches)


showMenu : Bool -> Autocomplete -> Autocomplete
showMenu bool auto =
    fst (updateAutocomplete (ShowMenu bool) auto)


setValue : String -> Autocomplete -> Autocomplete
setValue value auto =
    fst (updateAutocomplete (SetValue value) auto)


setItems : List String -> Autocomplete -> Autocomplete
setItems items auto =
    fst (updateAutocomplete (UpdateItems items) auto)


setLoading : Bool -> Autocomplete -> Autocomplete
setLoading bool auto =
    fst (update (SetLoading bool) auto)


type MenuNavigation
    = Previous
    | Next
    | Select


navigateMenu : MenuNavigation -> Autocomplete -> Msg
navigateMenu navigation (Autocomplete model) =
    case navigation of
        Previous ->
            ChangeSelection (model.selectedItemIndex - 1)

        Next ->
            ChangeSelection (model.selectedItemIndex + 1)

        Select ->
            Complete


getSelectedItem : Autocomplete -> Text
getSelectedItem (Autocomplete model) =
    let
        maybeSelectedItem =
            List.drop model.selectedItemIndex model.matches
                |> List.head
    in
        case maybeSelectedItem of
            Just item ->
                item

            Nothing ->
                model.value


getCurrentValue : Autocomplete -> String
getCurrentValue (Autocomplete model) =
    model.value


defaultStatus : Status
defaultStatus =
    { completed = False
    , valueChanged = False
    , selectionChanged = False
    }
