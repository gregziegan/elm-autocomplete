module Main (..) where

import StartApp.Simple
import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import String
import AtMention exposing (AtMention)


type alias Model =
  { mentions : Dict Position AtMention
  , value : String
  , currentMention : Maybe Position
  }


init : Model
init =
  { mentions = Dict.empty
  , value = ""
  , currentMention = Nothing
  }


type alias Position =
  Int


type Action
  = AtMention AtMention.Action Position AtMention
  | SetValue String


update : Action -> Model -> Model
update action model =
  case action of
    AtMention act pos mention ->
      { model
        | mentions = Dict.insert pos (AtMention.update act mention) model.mentions
      }

    SetValue value ->
      let
        -- getCurrentMention =
        --   case model.currentMention of
        --     Just pos ->
        --       getMention pos
        --     Nothing ->
        --       setCurrentMention
        getMention pos =
          Maybe.withDefault AtMention.init (Dict.get pos model.mentions)

        getMentionLength mention =
          AtMention.getValue mention
            |> String.length

        getNewMentionValue pos =
          String.slice pos (pos + (getMentionLength <| getMention pos) + 1) value

        updateMentions =
          let
            position =
              Maybe.withDefault (String.length value) model.currentMention
          in
            case model.currentMention of
              Just pos ->
                (AtMention.setValue (Debug.log "newValue" (getNewMentionValue pos)) (getMention pos))
                  |> AtMention.showMenu True
                  |> (\mention -> ( (Dict.insert position mention model.mentions), Just pos ))

              Nothing ->
                if String.endsWith "@" value then
                  ( Dict.insert position AtMention.init model.mentions, Just position )
                else
                  ( model.mentions, model.currentMention )
      in
        { model
          | value = value
          , mentions = (fst updateMentions)
          , currentMention = Debug.log "currentMention" (snd updateMentions)
        }


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ textarea
        [ on "input" targetValue (Signal.message address << SetValue)
        ]
        []
    , case model.currentMention of
        Just pos ->
          let
            mention =
              Maybe.withDefault AtMention.init (Dict.get pos model.mentions)
          in
            AtMention.view (Signal.forwardTo address (\act -> AtMention act pos mention)) mention

        Nothing ->
          div [] []
    ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
