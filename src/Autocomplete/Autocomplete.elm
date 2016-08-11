module Autocomplete.Autocomplete
    exposing
        ( State
        , empty
        , reset
        , Msg
        , UpdateConfig
        , updateConfig
        , update
        , view
        , Config
        , HtmlDetails
        , config
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
        , onMouseEnter : Maybe msg
        , onMouseLeave : Maybe msg
        , onMouseClick : Maybe msg
        }


updateConfig :
    { onKeyDown : KeyCode -> Bool
    , onChoose : String -> msg
    , onKeyChange : KeyCode -> msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : Maybe msg
    , onMouseLeave : Maybe msg
    , onMouseClick : Maybe msg
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
update (UpdateConfig config) msg (State state) =
    case msg of
        KeyChange keyCode id ->
            ( State { key = Just id, mouse = state.mouse }
            , Just
                (if config.onKeyDown keyCode then
                    config.onChoose id
                 else
                    config.onKeyChange keyCode
                )
            )

        WentTooLow ->
            ( State { key = Nothing, mouse = state.mouse }
            , config.onTooLow
            )

        WentTooFar ->
            ( State { key = Nothing, mouse = state.mouse }
            , config.onTooHigh
            )

        MouseEnter id ->
            ( State { key = state.key, mouse = Just id }
            , config.onMouseEnter
            )

        MouseLeave id ->
            ( State { key = state.key, mouse = Just id }
            , config.onMouseLeave
            )

        MouseClick id ->
            ( State { key = state.key, mouse = Just id }
            , config.onMouseClick
            )

        NoOp ->
            ( State state, Nothing )


hasSelection : State -> Bool
hasSelection (State { key, mouse }) =
    (Maybe.map2 (\k m -> True) key mouse) == Just True


getPreviousItemId : Config a -> List a -> String -> String
getPreviousItemId config data id =
    Maybe.withDefault id <| List.foldr (getPrevious config id) Nothing data


getPrevious : Config a -> String -> a -> Maybe String -> Maybe String
getPrevious (Config { toId }) id datum resultId =
    if (toId datum) == id then
        Just id
    else if (Maybe.withDefault "" resultId) == id then
        Just (toId datum)
    else
        resultId


getNextItemId : Config a -> List a -> String -> String
getNextItemId config data id =
    Maybe.withDefault id <| List.foldl (getPrevious config id) Nothing data


view : Config a -> Int -> State -> List a -> Html Msg
view (Config config) howManyToShow (State state) data =
    let
        dec =
            (Json.customDecoder Html.Events.keyCode
                (\code ->
                    case state.key of
                        Nothing ->
                            Err "not handling this"

                        Just id ->
                            if code == 38 then
                                Ok (KeyChange code <| getPreviousItemId (Config config) data id)
                            else if code == 40 then
                                Ok (KeyChange code <| getNextItemId (Config config) data id)
                            else
                                Err "not handling that key"
                )
            )

        customUlAttr =
            List.append (List.map trickyMap config.ul)
                [ Html.Events.on "keyDown" dec ]

        customLiAttr id attributes =
            List.append (List.map trickyMap attributes)
                [ Html.Events.onMouseEnter (MouseEnter id)
                , Html.Events.onMouseLeave (MouseLeave id)
                , Html.Events.onClick (MouseClick id)
                ]

        customListItem datum =
            ( config.li (hasSelection (State state)) datum, config.toId datum )

        customLi ( listItemData, id ) =
            Html.li (customLiAttr id listItemData.attributes)
                (List.map (Html.App.map (\html -> NoOp)) listItemData.children)
    in
        Html.ul customUlAttr
            (List.map (customLi << customListItem) data
                |> List.drop howManyToShow
            )


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type Config data
    = Config
        { toId : data -> String
        , ul : List (Attribute Never)
        , li : Bool -> data -> HtmlDetails Never
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
config :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : Bool -> data -> HtmlDetails Never
    }
    -> Config data
config { toId, ul, li } =
    Config
        { toId = toId
        , ul = ul
        , li = li
        }
