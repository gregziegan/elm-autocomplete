module Autocomplete.Autocomplete
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
        , viewWithSections
        , ViewConfig
        , ViewWithSectionsConfig
        , HtmlDetails
        , viewConfig
        , viewWithSectionsConfig
        , SectionConfig
        , sectionConfig
        , subscription
        )

import Char exposing (KeyCode)
import Html exposing (Html, Attribute)
import Html.App
import Html.Keyed
import Html.Events
import Keyboard
import Native.Tricks


trickyMap : Attribute Never -> Attribute Msg
trickyMap =
    Native.Tricks.trickyMap



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


reset : State -> State
reset { key, mouse } =
    { key = Nothing, mouse = mouse }


resetToFirstItem : List data -> (data -> String) -> State -> State
resetToFirstItem data toId state =
    let
        setFirstItem datum newState =
            { newState | key = Just <| toId datum }
    in
        case List.head data of
            Nothing ->
                reset state

            Just datum ->
                reset state
                    |> setFirstItem datum



-- UPDATE


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub Msg
subscription =
    Keyboard.downs KeyDown


type Msg
    = KeyDown KeyCode
    | WentTooLow
    | WentTooHigh
    | MouseEnter String
    | MouseLeave String
    | MouseClick String
    | NoOp


type alias UpdateConfig msg data =
    { onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , toId : data -> String
    }


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
updateConfig { onKeyDown, onTooLow, onTooHigh, onMouseEnter, onMouseLeave, onMouseClick, toId } =
    { onKeyDown = onKeyDown
    , onTooLow = onTooLow
    , onTooHigh = onTooHigh
    , onMouseEnter = onMouseEnter
    , onMouseLeave = onMouseLeave
    , onMouseClick = onMouseClick
    , toId = toId
    }


update : UpdateConfig msg data -> Msg -> State -> List data -> Int -> ( State, Maybe msg )
update config msg state data howManyToShow =
    case msg of
        KeyDown keyCode ->
            let
                boundedList =
                    List.map config.toId data
                        |> List.take howManyToShow

                newKey =
                    navigateWithKey keyCode boundedList state.key
            in
                if newKey == state.key && keyCode == 38 then
                    update config WentTooHigh state data howManyToShow
                else if newKey == state.key && keyCode == 40 then
                    update config WentTooLow state data howManyToShow
                else
                    ( { state | key = newKey }
                    , config.onKeyDown keyCode newKey
                    )

        WentTooLow ->
            ( state
            , config.onTooLow
            )

        WentTooHigh ->
            ( state
            , config.onTooHigh
            )

        MouseEnter id ->
            ( { key = state.key, mouse = Just id }
            , config.onMouseEnter id
            )

        MouseLeave id ->
            ( { key = state.key, mouse = Just id }
            , config.onMouseLeave id
            )

        MouseClick id ->
            ( { key = state.key, mouse = Just id }
            , config.onMouseClick id
            )

        NoOp ->
            ( state, Nothing )


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
            case maybeId of
                Nothing ->
                    case List.head ids of
                        Nothing ->
                            Nothing

                        Just firstId ->
                            Just firstId

                Just key ->
                    Just <| getNextItemId ids key

        _ ->
            maybeId


view : ViewConfig data -> Int -> State -> List data -> Html Msg
view config howManyToShow state data =
    viewList config howManyToShow state data


viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State -> List sectionData -> Html Msg
viewWithSections config howManyToShow state sections =
    let
        getKeyedItems section =
            ( config.section.toId section, viewSection config state section )
    in
        Html.Keyed.ul (List.map trickyMap config.section.ul)
            (List.map getKeyedItems sections)


viewSection : ViewWithSectionsConfig data sectionData -> State -> sectionData -> Html Msg
viewSection config state section =
    let
        sectionNode =
            config.section.li section

        attributes =
            List.map trickyMap sectionNode.attributes

        customChildren =
            List.map (Html.App.map (\html -> NoOp)) sectionNode.children

        getKeyedItems datum =
            ( config.toId datum, viewData config state datum )

        viewItemList =
            Html.Keyed.ul (List.map trickyMap config.ul)
                (config.section.getData section
                    |> List.map getKeyedItems
                )

        children =
            List.append customChildren [ viewItemList ]
    in
        Html.li attributes
            [ Html.node sectionNode.nodeType attributes children ]


viewData : ViewWithSectionsConfig data sectionData -> State -> data -> Html Msg
viewData { toId, li } { key, mouse } data =
    let
        id =
            toId data

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            (List.map trickyMap listItemData.attributes)

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter id)
                , Html.Events.onMouseLeave (MouseLeave id)
                , Html.Events.onClick (MouseClick id)
                ]

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
        Html.li customLiAttr
            (List.map (Html.App.map (\html -> NoOp)) listItemData.children)


viewList : ViewConfig data -> Int -> State -> List data -> Html Msg
viewList config howManyToShow state data =
    let
        customUlAttr =
            List.map trickyMap config.ul

        getKeyedItems datum =
            ( config.toId datum, viewItem config state datum )
    in
        Html.Keyed.ul customUlAttr
            (List.take howManyToShow data
                |> List.map getKeyedItems
            )


viewItem : ViewConfig data -> State -> data -> Html Msg
viewItem { toId, li } { key, mouse } data =
    let
        id =
            toId data

        listItemData =
            li (isSelected key) (isSelected mouse) data

        customAttributes =
            (List.map trickyMap listItemData.attributes)

        customLiAttr =
            List.append customAttributes
                [ Html.Events.onMouseEnter (MouseEnter id)
                , Html.Events.onMouseLeave (MouseLeave id)
                , Html.Events.onClick (MouseClick id)
                ]

        isSelected maybeId =
            case maybeId of
                Just someId ->
                    someId == id

                Nothing ->
                    False
    in
        Html.li customLiAttr
            (List.map (Html.App.map (\html -> NoOp)) listItemData.children)


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig data =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    }


type alias ViewWithSectionsConfig data sectionData =
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
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


{-| Create the `Config` for your `view` function. Everything you need to
render your columns efficiently and handle selection of columns.
Say we have a `List Person` that we want to show as a table. The table should
have a column for name and age. We would create a `Config` like this:
    import Table
    type Msg = NewTableState State | ...
    config : Table.Config Person Msg
    config =
      Table.config
        { toId = .name
        , toMsg = NewTableState
        , columns =
            [ Table.stringColumn "Name" .name
            , Table.intColumn "Age" .age
            ]
        }
You provide the following information in your table configuration:
  - `toId` &mdash; turn a `Person` into a unique ID. This lets us use
  [`Html.Keyed`][keyed] under the hood to make resorts faster.
  - `columns` &mdash; specify some columns to show.
  - `toMsg` &mdash; a way send new table states to your app as messages.
See the [examples][] to get a better feel for this!
[keyed]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Keyed
[examples]: https://github.com/evancz/elm-sortable-table/tree/master/examples
-}
viewConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    }
    -> ViewConfig data
viewConfig { toId, ul, li, input } =
    { toId = toId
    , ul = ul
    , li = li
    , input = input
    }


viewWithSectionsConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , input : List (Attribute Never)
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig { toId, ul, li, input, section } =
    { toId = toId
    , ul = ul
    , li = li
    , input = input
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
