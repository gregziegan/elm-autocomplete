module Autocomplete.View exposing (viewMenu)

import Autocomplete.DefaultStyles as DefaultStyles
import Autocomplete.Model as Autocomplete exposing (Model, Msg)
import Autocomplete.Config exposing (Text, Index, InputValue)
import Autocomplete.Styling as Styling
import Html exposing (..)
import Html.Attributes exposing (classList, style)
import Html.Events exposing (..)


viewItem : Model -> Text -> Index -> Html Msg
viewItem  model item index =
  li
    [ if model.config.useDefaultStyles then
        style DefaultStyles.itemStyles
      else
        classList <| model.config.getClasses Styling.Item
    , onMouseEnter  (Autocomplete.ChangeSelection index)
    ]
    [ model.config.itemHtmlFn item ]


viewSelectedItem : Model -> Text -> Html Msg
viewSelectedItem  model item =
  li
    [ if model.config.useDefaultStyles then
        style DefaultStyles.selectedItemStyles
      else
        classList <| model.config.getClasses Styling.SelectedItem
    , onClick  Autocomplete.Complete
    ]
    [ model.config.itemHtmlFn item ]


viewMenu : Model -> Html Msg
viewMenu model =
  div
    [ if model.config.useDefaultStyles then
        style DefaultStyles.menuStyles
      else
        classList <| model.config.getClasses Styling.Menu
    ]
    [ viewList  model ]


viewList : Model -> Html Msg
viewList  model =
  let
    getItemView index item =
      if index == model.selectedItemIndex then
        viewSelectedItem  model item
      else
        viewItem  model item index
  in
    ul
      [ if model.config.useDefaultStyles then
          style DefaultStyles.listStyles
        else
          classList <| model.config.getClasses Styling.List
      ]
      (List.indexedMap getItemView model.matches)
