module Autocomplete.Autocomplete
    exposing
        ( view
        , update
        , subscription
        , viewConfig
        , updateConfig
        , State
        , empty
        , reset
        , resetToFirstItem
        , resetToLastItem
        , KeySelected
        , MouseSelected
        , Msg
        , ViewConfig
        , UpdateConfig
        , HtmlDetails
        , viewWithSections
        , sectionConfig
        , viewWithSectionsConfig
        , SectionNode
        , SectionConfig
        , ViewWithSectionsConfig
        )

import Char exposing (KeyCode)
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Keyed
import Html.Events
import Keyboard


-- MODEL


type alias State =
    { key : Maybe String
    , mouse : Maybe String
    }


type alias KeySelected =
    Bool


type alias MouseSelected =
    Bool


empty : State
empty =
    { key = Nothing, mouse = Nothing }


reset : UpdateConfig msg data -> State -> State
reset { separateSelections } { key, mouse } =
    if separateSelections then
        { key = Nothing, mouse = mouse }
    else
        empty


resetToFirstItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToFirstItem config data howManyToShow state =
    resetToFirst config (List.take howManyToShow data) state


resetToFirst : UpdateConfig msg data -> List data -> State -> State
resetToFirst config data state =
    let
        { toId, separateSelections } =
            config

        setFirstItem datum newState =
            { newState | key = Just <| toId datum }
    in
        case List.head data of
            Nothing ->
                empty

            Just datum ->
                if separateSelections then
                    reset config state
                        |> setFirstItem datum
                else
                    empty
                        |> setFirstItem datum


resetToLastItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToLastItem config data howManyToShow state =
    let
        reversedData =
            List.reverse <| List.take howManyToShow data
    in
        resetToFirst config reversedData state



-- UPDATE


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub (Msg data)
subscription =
    Keyboard.downs KeyDown


type Msg data
    = KeyDown KeyCode
    | WentTooLow
    | WentTooHigh
    | MouseEnter data
    | MouseLeave data
    | MouseClick data
    | NoOp


type alias UpdateConfig msg data =
    { onKeyDown : KeyCode -> Maybe data -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : data -> Maybe msg
    , onMouseLeave : data -> Maybe msg
    , onMouseClick : data -> Maybe msg
    , toId : data -> String
    , separateSelections : Bool
    }


updateConfig :
    { toId : data -> String
    , onKeyDown : KeyCode -> Maybe data -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : data -> Maybe msg
    , onMouseLeave : data -> Maybe msg
    , onMouseClick : data -> Maybe msg
    , separateSelections : Bool
    }
    -> UpdateConfig msg data
updateConfig { toId, onKeyDown, onTooLow, onTooHigh, onMouseEnter, onMouseLeave, onMouseClick, separateSelections } =
    { toId = toId
    , onKeyDown = onKeyDown
    , onTooLow = onTooLow
    , onTooHigh = onTooHigh
    , onMouseEnter = onMouseEnter
    , onMouseLeave = onMouseLeave
    , onMouseClick = onMouseClick
    , separateSelections = separateSelections
    }


update : UpdateConfig msg data -> Msg data -> Int -> State -> List data -> ( State, Maybe msg )
update config msg howManyToShow state data =
    case msg of
        KeyDown keyCode ->
            let
                boundedList =
                    List.map config.toId data
                        |> List.take howManyToShow

                newKey =
                    navigateWithKey keyCode boundedList state.key

                item =
                    newKey
                        |> Maybe.andThen
                            (\key ->
                                data
                                    |> List.filter (\x -> config.toId x == key)
                                    |> List.head
                            )
            in
                if newKey == state.key && keyCode == 38 then
                    update config WentTooHigh howManyToShow state data
                else if newKey == state.key && keyCode == 40 then
                    update config WentTooLow howManyToShow state data
                else if config.separateSelections then
                    ( { state | key = newKey }
                    , config.onKeyDown keyCode item
                    )
                else
                    ( { key = newKey, mouse = newKey }
                    , config.onKeyDown keyCode item
                    )

        WentTooLow ->
            ( state
            , config.onTooLow
            )

        WentTooHigh ->
            ( state
            , config.onTooHigh
            )

        MouseEnter data ->
            ( resetMouseStateWithId config.separateSelections (config.toId data) state
            , config.onMouseEnter data
            )

        MouseLeave data ->
            ( resetMouseStateWithId config.separateSelections (config.toId data) state
            , config.onMouseLeave data
            )

        MouseClick data ->
            ( resetMouseStateWithId config.separateSelections (config.toId data) state
            , config.onMouseClick data
            )

        NoOp ->
            ( state, Nothing )


resetMouseStateWithId : Bool -> String -> State -> State
resetMouseStateWithId separateSelections id state =
    if separateSelections then
        { key = state.key, mouse = Just id }
    else
        { key = Just id, mouse = Just id }


getPreviousItemId : List String -> String -> String
getPreviousItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldr (getPrevious selectedId) Nothing ids


getPrevious : String -> String -> Maybe String -> Maybe String
getPrevious id selectedId resultId =
    if selectedId == id then
        Just id
    else if (Maybe.withDefault "" resultId) == id then
        Just selectedId
    else
        resultId


getNextItemId : List String -> String -> String
getNextItemId ids selectedId =
    Maybe.withDefault selectedId <| List.foldl (getPrevious selectedId) Nothing ids


navigateWithKey : Int -> List String -> Maybe String -> Maybe String
navigateWithKey code ids maybeId =
    case code of
        38 ->
            Maybe.map (getPreviousItemId ids) maybeId

        40 ->
            Maybe.map (getNextItemId ids) maybeId

        _ ->
            maybeId


view : ViewConfig data -> Int -> State -> List data -> Html (Msg data)
view config howManyToShow state data =
    viewList config howManyToShow state data


viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State -> List sectionData -> Html (Msg data)
viewWithSections config howManyToShow state sections =
    let
        getKeyedItems section =
            ( config.section.toId section, viewSection config state section )
    in
        Html.Keyed.ul (List.map mapNeverToMsg config.section.ul)
            (List.map getKeyedItems sections)


viewSection : ViewWithSectionsConfig data sectionData -> State -> sectionData -> Html (Msg data)
viewSection config state section =
    let
        sectionNode =
            config.section.li section

        attributes =
            List.map mapNeverToMsg sectionNode.attributes

        customChildren =
            List.map (Html.map (\html -> NoOp)) sectionNode.children

        getKeyedItems datum =
            ( config.toId datum, viewData config state datum )

        viewItemList =
            Html.Keyed.ul (List.map mapNeverToMsg config.ul)
                (config.section.getData section
                    |> List.map getKeyedItems
                )

        children =
            List.append customChildren [ viewItemList ]
    in
        Html.li attributes
            [ Html.node sectionNode.nodeType attributes children ]


viewData : ViewWithSectionsConfig data sectionData -> State -> data -> Html (Msg data)
viewData { toId, li } { key, mouse } data =
    let
        id =
            toId data

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            (List.map mapNeverToMsg listItemData.attributes)

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter data)
                , Html.Events.onMouseLeave (MouseLeave data)
                , Html.Events.onClick (MouseClick data)
                ]

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
        Html.li customLiAttr
            (List.map (Html.map (\html -> NoOp)) listItemData.children)


viewList : ViewConfig data -> Int -> State -> List data -> Html (Msg data)
viewList config howManyToShow state data =
    let
        customUlAttr =
            List.map mapNeverToMsg config.ul

        getKeyedItems datum =
            ( config.toId datum, viewItem config state datum )
    in
        Html.Keyed.ul customUlAttr
            (List.take howManyToShow data
                |> List.map getKeyedItems
            )


viewItem : ViewConfig data -> State -> data -> Html (Msg data)
viewItem { toId, li } { key, mouse } data =
    let
        id =
            toId data

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            (List.map mapNeverToMsg listItemData.attributes)

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter data)
                , Html.Events.onMouseLeave (MouseLeave data)
                , Html.Events.onClick (MouseClick data)
                ]

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
        Html.li customLiAttr
            (List.map (Html.map (\html -> NoOp)) listItemData.children)


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig data =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }


type alias ViewWithSectionsConfig data sectionData =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }


type alias SectionConfig data sectionData =
    { toId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }


type alias SectionNode msg =
    { nodeType : String
    , attributes : List (Attribute msg)
    , children : List (Html msg)
    }


viewConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }
    -> ViewConfig data
viewConfig { toId, ul, li } =
    { toId = toId
    , ul = ul
    , li = li
    }


viewWithSectionsConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig { toId, ul, li, section } =
    { toId = toId
    , ul = ul
    , li = li
    , section = section
    }


sectionConfig :
    { toId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }
    -> SectionConfig data sectionData
sectionConfig { toId, getData, ul, li } =
    { toId = toId
    , getData = getData
    , ul = ul
    , li = li
    }



-- HELPERS


mapNeverToMsg : Attribute Never -> Attribute (Msg data)
mapNeverToMsg msg =
    Html.Attributes.map (\_ -> NoOp) msg
