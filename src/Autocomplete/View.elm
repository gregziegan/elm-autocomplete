module Autocomplete.View (viewMenu) where

import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Update as Autocomplete exposing (Action)
import Autocomplete.Model exposing (Model)
import Autocomplete.Config exposing (Text, Index, InputValue)
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (classList)
import Html.Events exposing (..)
import Signal exposing (Address)


viewItem : Address Action -> Model -> Text -> Index -> Html
viewItem address model item index =
  li
    [ if model.config.useDefaultStyles then
        DefaultStyles.itemStyle
      else
        classList <| model.config.getClasses Styling.Item
    , onMouseEnter address (Autocomplete.ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Address Action -> Model -> Text -> Html
viewSelectedItem address model item =
  li
    [ if model.config.useDefaultStyles then
        DefaultStyles.selectedItemStyle
      else
        classList <| model.config.getClasses Styling.SelectedItem
    , onClick address Autocomplete.Complete
    ]
    [ model.config.itemHtmlFn item ]


viewMenu : Address Action -> Model -> Html
viewMenu address model =
  div
    [ if model.config.useDefaultStyles then
        DefaultStyles.menuStyle
      else
        classList <| model.config.getClasses Styling.Menu
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
      [ if model.config.useDefaultStyles then
          DefaultStyles.listStyle
        else
          classList <| model.config.getClasses Styling.List
      ]
      (List.indexedMap getItemView model.matches)
