module Main (..) where

import StartApp.Simple
import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Set
import String
import AtMention exposing (AtMention)


type alias Model =
  { mentions : Dict Position AtMention
  , value : String
  }


init : Model
init =
  { mentions = Dict.empty
  , value = ""
  }


type alias Position =
  Int


type Action
  = AtMention AtMention.Action Position
  | SetValue String


addNewMention : Model -> String -> Dict Position AtMention
addNewMention model newValue =
  let
    oldMentions =
      String.indices "@" model.value

    currentMentions =
      String.indices "@" newValue

    newMentionsSet =
      Set.diff (Set.fromList currentMentions) (Set.fromList oldMentions)

    newMentions =
      Set.toList newMentionsSet
        |> List.map (\pos -> ( pos, AtMention.init ))
        |> Dict.fromList
  in
    Dict.union newMentions model.mentions


update : Action -> Model -> Model
update action model =
  case action of
    AtMention act pos ->
      let
        maybeMention =
          Dict.get pos model.mentions
      in
        case maybeMention of
          Just mention ->
            { model | mentions = Dict.insert pos (AtMention.update act mention) model.mentions }

          Nothing ->
            model

    SetValue value ->
      let
        updatedMentions =
          addNewMention model value

        mentionList =
          Debug.log "mentions" (Dict.keys updatedMentions)
      in
        { model
          | value = value
          , mentions = updatedMentions
        }


view : Signal.Address Action -> Model -> Html
view address model =
  div
    []
    [ textarea
        [ on "input" targetValue (Signal.message address << SetValue)
        ]
        []
    ]


main : Signal Html.Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }
