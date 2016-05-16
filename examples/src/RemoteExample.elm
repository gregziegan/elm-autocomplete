module Main exposing (..)

import Html exposing (..)
import Html.App as Html exposing (map)
import Html.Attributes exposing (..)
import Autocomplete
import Autocomplete.Config
import Task exposing (Task)
import Http
import String

type alias Model =
  { remoteItems : List String
  , autocomplete : Autocomplete.Autocomplete
  }

init : (Model, Cmd Msg)
init =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setLoadingDisplay (img [ src "assets/loading.svg" ] [])
  in
    ({ remoteItems = []
      , autocomplete = Autocomplete.initWithConfig [] config
      }, Cmd.none)

type Msg
  = UpdateAutocomplete Autocomplete.Msg
  | FetchItemsSuccess (List String)
  | FetchItemsFail Http.Error

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateAutocomplete autoMsg ->
      let
        ( updatedAutocomplete, status ) = Autocomplete.update autoMsg model.autocomplete
      in
        if status.valueChanged && List.isEmpty model.remoteItems then
          ({ model
                | autocomplete  = Autocomplete.setLoading True updatedAutocomplete }
            ,  getMoreItems <| Autocomplete.getSelectedItem updatedAutocomplete
          )
        else
          ({ model | autocomplete = updatedAutocomplete }, Cmd.none)

    FetchItemsSuccess items ->
      ({ model
          | remoteItems = items
          , autocomplete = Autocomplete.setItems items model.autocomplete
              |> Autocomplete.setLoading False
        }, Cmd.none )

    FetchItemsFail _ ->
      ( model, Cmd.none )

fetchItems : String -> Task Http.Error (List String)
fetchItems url =
  Http.url url []
    |> Http.getString
    |> Task.map (\body -> String.lines body)


getMoreItems : String -> Cmd Msg
getMoreItems value =
  let
    url = "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"
  in
    Task.perform FetchItemsFail FetchItemsSuccess (fetchItems url)


view : Model -> Html Msg
view model =
  map UpdateAutocomplete (Autocomplete.view model.autocomplete)

main : Program Never
main =
    Html.program
      { init = init
      , update = update
      , view = view
      , subscriptions = (\_ -> Sub.none)
      }
