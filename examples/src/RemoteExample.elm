module Main exposing (..)

import Html exposing (..)
import Html.App as Html exposing (map)
import Html.Attributes exposing (..)
import Autocomplete exposing (init, update, view)
import Autocomplete.Config
import Task exposing (Task)
import Http
import String

fetchMoreItems : String -> Task Never (List String)
fetchMoreItems url =
  Http.url url []
    |> Http.getString
    |> Task.toMaybe
    |> Task.map responseToItems


responseToItems : Maybe String -> List String
responseToItems maybeString =
  case maybeString of
    Just string ->
      String.lines string

    Nothing ->
      []


getItemsTask : String -> Int -> Task String (List String)
getItemsTask value index =
  fetchMoreItems "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"
    |> Task.mapError (\x -> "Connection Failed")


main : Program Never
main =
  let
    config =
      Autocomplete.Config.defaultConfig
        |> Autocomplete.Config.setLoadingDisplay (img [ src "assets/loading.svg" ] [])

    updateAutocomplete act model =
      let
        ( updatedAutocomplete, effects, complete ) =
          update act model
      in
        ( updatedAutocomplete, effects )
  in
    Html.program
      { init = init [] getItemsTask
      , update = updateAutocomplete
      , view = view
      , subscriptions = (\_ -> Sub.none)
      }
