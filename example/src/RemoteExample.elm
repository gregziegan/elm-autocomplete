module Main (..) where

import Effects exposing (Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Autocomplete exposing (init, update, view)
import StartApp
import Task exposing (Task)
import Http
import String


fetchMoreItems : String -> Task Effects.Never (List String)
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


getItemsTask : String -> Int -> Task Effects.Never (List String)
getItemsTask value index =
  fetchMoreItems "https://raw.githubusercontent.com/first20hours/google-10000-english/master/20k.txt"


app =
  let
    config =
      Autocomplete.defaultConfig
        |> Autocomplete.setLoadingDisplay (img [ src "assets/loading.svg" ] [])
  in
    StartApp.start
      { init = init [] getItemsTask
      , update = update
      , view = view
      , inputs = []
      }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
