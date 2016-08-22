module Autocomplete.Autocomplete
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
        , subscription
        )

import Char exposing (KeyCode)
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.App
import Html.Events
import Json.Decode as Json
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



-- UPDATE


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : List String -> Sub Msg
subscription ids =
    Keyboard.downs (KeyDown ids)


type Msg
    = KeyDown (List String) KeyCode
    | Choose String
    | WentTooLow
    | WentTooFar
    | MouseEnter String
    | MouseLeave String
    | MouseClick String
    | NoOp


type alias UpdateConfig msg =
    { onChoose : String -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe (String -> msg)
    , onMouseLeave : Maybe (String -> msg)
    , onMouseClick : Maybe (String -> msg)
    }


updateConfig :
    { onChoose : String -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe (String -> msg)
    , onMouseLeave : Maybe (String -> msg)
    , onMouseClick : Maybe (String -> msg)
    }
    -> UpdateConfig msg
updateConfig { onChoose, onTooLow, onTooHigh, onMouseEnter, onMouseLeave, onMouseClick } =
    { onChoose = onChoose
    , onTooLow = onTooLow
    , onTooHigh = onTooHigh
    , onMouseEnter = onMouseEnter
    , onMouseLeave = onMouseLeave
    , onMouseClick = onMouseClick
    }


update : UpdateConfig msg -> Msg -> State -> ( State, Maybe msg )
update config msg { key, mouse } =
    case msg of
        KeyDown ids keyCode ->
            ( { key = navigateWithKey keyCode ids key, mouse = mouse }
            , Nothing
            )

        Choose id ->
            ( { key = key, mouse = mouse }, Just <| config.onChoose id )

        WentTooLow ->
            ( { key = Nothing, mouse = mouse }
            , config.onTooLow
            )

        WentTooFar ->
            ( { key = Nothing, mouse = mouse }
            , config.onTooHigh
            )

        MouseEnter id ->
            ( { key = key, mouse = Just id }
            , callMaybeFn config.onMouseEnter id
            )

        MouseLeave id ->
            ( { key = key, mouse = Just id }
            , callMaybeFn config.onMouseLeave id
            )

        MouseClick id ->
            ( { key = key, mouse = Just id }
            , callMaybeFn config.onMouseClick id
            )

        NoOp ->
            ( { key = key, mouse = mouse }, Nothing )


callMaybeFn : Maybe (a -> msg) -> a -> Maybe msg
callMaybeFn maybeFn id =
    case maybeFn of
        Nothing ->
            Nothing

        Just fn ->
            Just <| fn id


getPreviousItemId : List String -> String -> String
getPreviousItemId ids curId =
    Maybe.withDefault curId <| List.foldr (getPrevious curId) Nothing ids


getPrevious : String -> String -> Maybe String -> Maybe String
getPrevious id curId resultId =
    if curId == id then
        Just id
    else if (Maybe.withDefault "" resultId) == id then
        Just curId
    else
        resultId


getNextItemId : List String -> String -> String
getNextItemId ids curId =
    Maybe.withDefault curId <| List.foldl (getPrevious curId) Nothing ids


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



-- case maybeId of
--     Nothing ->
--         Nothing
--
--     Just id ->
--         if List.any ((==) id) <| List.take 5 ids then
--             maybeId
--         else
--             List.head ids


view : ViewConfig a -> Int -> State -> List a -> Html Msg
view config howManyToShow state data =
    viewList config howManyToShow state data


viewList : ViewConfig a -> Int -> State -> List a -> Html Msg
viewList config howManyToShow state data =
    let
        customUlAttr =
            List.map trickyMap config.ul
    in
        Html.ul customUlAttr
            (List.take howManyToShow data
                |> List.map (viewItem config state)
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
    , isChooseKey : KeyCode -> Bool
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
    , isChooseKey : KeyCode -> Bool
    }
    -> ViewConfig data
viewConfig { toId, ul, li, input, isChooseKey } =
    { toId = toId
    , ul = ul
    , li = li
    , input = input
    , isChooseKey = isChooseKey
    }
