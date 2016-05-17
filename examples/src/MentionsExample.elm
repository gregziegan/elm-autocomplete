port module Main exposing (..)

import Html exposing (..)
import Html.App as Html exposing (map)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json
import String
import AtMention exposing (AtMention)
import Autocomplete

type alias Model =
  { mentions : Dict Position AtMention
  , value : String
  , currentMentionPos : Maybe Position
  , caretPos: CaretPosition
  }

type alias CaretPosition =
  { top : Int
  , left : Int
  }

init : (Model, Cmd Msg)
init =
  ({ mentions = Dict.empty
  , value = ""
  , currentMentionPos = Nothing
  , caretPos = { top = 0, left = 0 }
  }, Cmd.none)


type alias Position =
  Int


type Msg
  = NoOp
  | AtMention AtMention.Msg Position AtMention
  | SetValue String
  | ToggleMenu Bool
  | UpdateCaretPosition CaretPosition

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none)

    AtMention mentionMsg pos mention ->
      let
        ( updatedMention, status ) =
          AtMention.update mentionMsg mention

        currentMentionLength =
          AtMention.getValue mention
            |> String.length

        startToMentionSlice =
          String.slice 0 pos model.value

        completedMentionValue =
          AtMention.getValue updatedMention

        mentionStartToEndSlice =
          String.slice (pos + currentMentionLength)  (String.length model.value) model.value

        newValue =
          startToMentionSlice ++ completedMentionValue ++ mentionStartToEndSlice
      in
        if status.completed then
          ({ model
            | mentions = Dict.insert pos updatedMention model.mentions
            , value = newValue
          }, Cmd.none)
        else
          ({ model
            | mentions = Dict.insert pos updatedMention model.mentions
          }, Cmd.none)

    SetValue value ->
       ( updateMentionValue model value, Cmd.none )

    ToggleMenu bool ->
      let
          updatedMention pos mentions =
            getMention pos mentions
              |> AtMention.showMenu bool
          updatedMentions pos mentions =
              Dict.insert pos (updatedMention pos mentions) mentions
      in
        case model.currentMentionPos of
          Just mentionPos ->
            ({ model |
                mentions  = updatedMentions mentionPos model.mentions
            }, Cmd.none )
          Nothing ->
            ( model, Cmd.none )

    UpdateCaretPosition caretPos ->
      ( { model | caretPos = caretPos }, Cmd.none )

updateMentionValue : Model -> String -> Model
updateMentionValue model value =
    let
      getMentionLength mention =
        AtMention.getValue mention
          |> String.length

      getNewMentionValue pos =
        String.slice pos (pos + (getMentionLength <| getMention pos model.mentions) + 1) value

      position =
        Maybe.withDefault (String.length value) model.currentMentionPos

      updateMentions mention pos =
        { model
          | value = value
          , mentions = Dict.insert position mention model.mentions
          , currentMentionPos = Just pos
        }
    in
      case model.currentMentionPos of
        Just pos ->
          if String.endsWith " " value then
            { model
              | value = value
              , mentions = Dict.remove pos model.mentions
              , currentMentionPos = Nothing
            }
          else
            (AtMention.setValue (getNewMentionValue pos)) (getMention pos model.mentions)
              |> (\mention -> updateMentions mention pos )

        Nothing ->
          if String.endsWith "@" value then
            { model
              | value = value
              , mentions = Dict.insert position AtMention.init model.mentions
              , currentMentionPos = Just position
            }
          else
            { model
              | value = value
              , mentions = model.mentions
              , currentMentionPos = model.currentMentionPos
            }

getMention : Position -> Dict Position AtMention -> AtMention
getMention pos mentions =
  Maybe.withDefault AtMention.init (Dict.get pos mentions)

view : Model -> Html Msg
view model =
    div []
          [ viewEditor model
          , case model.currentMentionPos of
              Just pos ->
                let
                  mention =
                    Maybe.withDefault AtMention.init (Dict.get pos model.mentions)
                in
                  div [ style [ ("top", toString model.caretPos.top ++ "px" ), ("left", toString model.caretPos.left ++ "px"), ("position", "absolute") ] ]
                   [ map (\act -> AtMention act pos mention) (AtMention.view mention) ]

              Nothing ->
                div [] []
            ]

viewEditor :  Model -> Html Msg
viewEditor model =
  let
    options =
      { preventDefault = True, stopPropagation = False }

    dec =
      (Json.customDecoder
        keyCode
        navigate
      )

    navigateMenu code pos mention =
      if code == 38 then
          Ok (AtMention (AtMention.NavigateMenu Autocomplete.Previous) pos mention)
      else if code == 40 then
          Ok (AtMention (AtMention.NavigateMenu Autocomplete.Next) pos mention)
      else if List.member code [ 9, 13 ] then
          Ok (AtMention (AtMention.NavigateMenu Autocomplete.Select) pos mention)
      else
          Err "not handling that key"

    navigate code =
      case model.currentMentionPos of
        Just pos ->
          case Dict.get pos model.mentions of
            Just mention ->
              navigateMenu code pos mention

            Nothing ->
              Err "not handling that key"

        Nothing ->
          Err "not handling that key"

    toggleMenu code =
      case code of
        27 ->
          ToggleMenu  False
        _ ->
          NoOp
  in
    div [ on "keydown" (Json.map toggleMenu keyCode) ]
          [ textarea
            [ onInput SetValue
            , onWithOptions "keydown" options dec
            , value model.value
            , class "editor"
            ]
            []
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  caretPosition UpdateCaretPosition

main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

port caretPosition : (CaretPosition -> msg) -> Sub msg
