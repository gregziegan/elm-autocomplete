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
        )

import Char exposing (KeyCode)
import Html exposing (Html, Attribute)
import Html.App
import Html.Events
import Json.Decode as Json
import Native.Tricks


trickyMap : Attribute Never -> Attribute Msg
trickyMap =
    Native.Tricks.trickyMap



-- MODEL


type State
    = State
        { key : Maybe String
        , mouse : Maybe String
        }


type alias KeySelected =
    Bool


type alias MouseSelected =
    Bool


empty : State
empty =
    State { key = Nothing, mouse = Nothing }


reset : State -> State
reset (State { key, mouse }) =
    State { key = Nothing, mouse = mouse }



-- UPDATE


type Msg
    = KeyChange KeyCode String
    | WentTooLow
    | WentTooFar
    | MouseEnter String
    | MouseLeave String
    | MouseClick String
    | NoOp


type UpdateConfig msg
    = UpdateConfig
        { onKeyDown : KeyCode -> Bool
        , onChoose : String -> msg
        , onKeyChange : KeyCode -> msg
        , onTooLow : Maybe msg
        , onTooHigh : Maybe msg
        , onMouseEnter : Maybe (String -> msg)
        , onMouseLeave : Maybe (String -> msg)
        , onMouseClick : Maybe (String -> msg)
        }


updateConfig :
    { onKeyDown : KeyCode -> Bool
    , onChoose : String -> msg
    , onKeyChange : KeyCode -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe (String -> msg)
    , onMouseLeave : Maybe (String -> msg)
    , onMouseClick : Maybe (String -> msg)
    }
    -> UpdateConfig msg
updateConfig { onKeyDown, onChoose, onKeyChange, onTooLow, onTooHigh, onMouseEnter, onMouseLeave, onMouseClick } =
    UpdateConfig
        { onKeyDown = onKeyDown
        , onChoose = onChoose
        , onKeyChange = onKeyChange
        , onTooLow = onTooLow
        , onTooHigh = onTooHigh
        , onMouseEnter = onMouseEnter
        , onMouseLeave = onMouseLeave
        , onMouseClick = onMouseClick
        }


update : UpdateConfig msg -> Msg -> State -> ( State, Maybe msg )
update (UpdateConfig config) msg (State { key, mouse }) =
    case Debug.log "msg" msg of
        KeyChange keyCode id ->
            ( State { key = Just id, mouse = mouse }
            , Just
                (if config.onKeyDown keyCode then
                    config.onChoose id
                 else
                    config.onKeyChange keyCode
                )
            )

        WentTooLow ->
            ( State { key = Nothing, mouse = mouse }
            , config.onTooLow
            )

        WentTooFar ->
            ( State { key = Nothing, mouse = mouse }
            , config.onTooHigh
            )

        MouseEnter id ->
            ( State { key = key, mouse = Just id }
            , callMaybeFn config.onMouseEnter id
            )

        MouseLeave id ->
            ( State { key = key, mouse = Just id }
            , callMaybeFn config.onMouseLeave id
            )

        MouseClick id ->
            ( State { key = key, mouse = Just id }
            , callMaybeFn config.onMouseClick id
            )

        NoOp ->
            ( State { key = key, mouse = mouse }, Nothing )


callMaybeFn : Maybe (a -> msg) -> a -> Maybe msg
callMaybeFn maybeFn id =
    case maybeFn of
        Nothing ->
            Nothing

        Just fn ->
            Just <| fn id


getPreviousItemId : ViewConfig a -> List a -> String -> String
getPreviousItemId config data id =
    Maybe.withDefault id <| List.foldr (getPrevious config id) Nothing data


getPrevious : ViewConfig a -> String -> a -> Maybe String -> Maybe String
getPrevious (ViewConfig { toId }) id datum resultId =
    if (toId datum) == id then
        Just id
    else if (Maybe.withDefault "" resultId) == id then
        Just (toId datum)
    else
        resultId


getNextItemId : ViewConfig a -> List a -> String -> String
getNextItemId config data id =
    Maybe.withDefault id <| List.foldl (getPrevious config id) Nothing data


navigateDecoder config state data =
    let
        decodeKeyCode code =
            case state.key of
                Nothing ->
                    keyEmptyDecoder code

                Just id ->
                    arrowKeyDecoder id code

        keyEmptyDecoder code =
            case List.head data of
                Nothing ->
                    Err "no navigation possible"

                Just item ->
                    if code == 40 then
                        config.toId item
                            |> KeyChange code
                            |> Ok
                    else
                        Err "not handling anything else"

        arrowKeyDecoder id code =
            if code == 38 then
                Ok (KeyChange code <| getPreviousItemId (ViewConfig config) data id)
            else if code == 40 then
                Ok (KeyChange code <| getNextItemId (ViewConfig config) data id)
            else
                Err "not handling that key"
    in
        Json.customDecoder Html.Events.keyCode decodeKeyCode


view : ViewConfig a -> Int -> State -> List a -> Html Msg
view (ViewConfig config) howManyToShow (State state) data =
    let
        customUlAttr =
            List.append (List.map trickyMap config.ul)
                [ Html.Events.on "keyDown" (navigateDecoder config state data) ]
    in
        Html.ul customUlAttr
            (List.map (viewItem (ViewConfig config) (State state)) data
                |> List.take howManyToShow
            )


viewItem : ViewConfig a -> State -> a -> Html Msg
viewItem (ViewConfig { toId, li }) (State { key, mouse }) data =
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


type ViewConfig data
    = ViewConfig
        { toId : data -> String
        , ul : List (Attribute Never)
        , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
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
    }
    -> ViewConfig data
viewConfig { toId, ul, li } =
    ViewConfig
        { toId = toId
        , ul = ul
        , li = li
        }
