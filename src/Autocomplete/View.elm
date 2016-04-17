module Autocomplete.View (viewMenu) where

import Autocomplete.Update as Autocomplete exposing (Action)
import Autocomplete.Model exposing (Model)
import Autocomplete.Config exposing (Text, Index, InputValue)
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)


viewItem : Address Action -> Model -> Text -> Index -> Html
viewItem address model item index =
  li
    [ model.config.styleViewFn Styling.Item
    , onMouseEnter address (Autocomplete.ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Address Action -> Model -> Text -> Html
viewSelectedItem address model item =
  li
    [ model.config.styleViewFn Styling.SelectedItem
    , onClick address Autocomplete.Complete
    ]
    [ model.config.itemHtmlFn item ]


viewMenu : Address Action -> Model -> Html
viewMenu address model =
  div
    [ model.config.styleViewFn Styling.Menu
    ]
    [ viewList address model ]


viewList : Address Action -> Model -> Html
viewList address model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem address model item
      else
        viewItem address model item index
  in
    ul
      [ model.config.styleViewFn Styling.List
      ]
      (List.indexedMap getItemView model.matches)
