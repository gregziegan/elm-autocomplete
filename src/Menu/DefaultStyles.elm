module Menu.DefaultStyles exposing (inputStyles, itemStyles, listStyles, menuStyles, selectedItemStyles)

import Html
import Html.Attributes as Attrs


menuStyles : List (Html.Attribute msg)
menuStyles =
    [ Attrs.style "position" "absolute"
    , Attrs.style "left" "5px"
    , Attrs.style "margin-top" "5px"
    , Attrs.style "background" "white"
    , Attrs.style "color" "black"
    , Attrs.style "border" "1px solid #DDD"
    , Attrs.style "border-radius" "3px"
    , Attrs.style "box-shadow" "0 0 5px rgba(0,0,0,0.1)"
    , Attrs.style "min-width" "120px"
    , Attrs.style "z-index" "11110"
    ]


selectedItemStyles : List (Html.Attribute msg)
selectedItemStyles =
    [ Attrs.style "background" "#3366FF"
    , Attrs.style "color" "white"
    , Attrs.style "display" "block"
    , Attrs.style "padding" "5px 10px"
    , Attrs.style "border-bottom" "1px solid #DDD"
    , Attrs.style "cursor" "pointer"
    ]


listStyles : List (Html.Attribute msg)
listStyles =
    [ Attrs.style "list-style" "none"
    , Attrs.style "padding" "0"
    , Attrs.style "margin" "auto"
    , Attrs.style "max-height" "200px"
    , Attrs.style "overflow-y" "auto"
    ]


itemStyles : List (Html.Attribute msg)
itemStyles =
    [ Attrs.style "display" "block"
    , Attrs.style "padding" "5px 10px"
    , Attrs.style "border-bottom" "1px solid #DDD"
    , Attrs.style "cursor" "pointer"
    ]


inputStyles : List (Html.Attribute msg)
inputStyles =
    [ Attrs.style "min-width" "120px"
    , Attrs.style "color" "black"
    , Attrs.style "position" "relative"
    , Attrs.style "display" "block"
    , Attrs.style "padding" "0.8em"
    , Attrs.style "font-size" "12px"
    ]
