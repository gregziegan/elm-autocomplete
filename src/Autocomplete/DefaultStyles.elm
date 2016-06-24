module Autocomplete.DefaultStyles exposing (..)


menuStyles : List ( String, String )
menuStyles =
    [ ( "position", "absolute" )
    , ( "left", "5px" )
    , ( "margin-top", "5px" )
    , ( "background", "white" )
    , ( "color", "black" )
    , ( "border", "1px solid #DDD" )
    , ( "border-radius", "3px" )
    , ( "box-shadow", "0 0 5px rgba(0,0,0,0.1)" )
    , ( "min-width", "120px" )
    , ( "z-index", "11110" )
    ]


selectedItemStyles : List ( String, String )
selectedItemStyles =
    [ ( "background", "#3366FF" )
    , ( "color", "white" )
    , ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


listStyles : List ( String, String )
listStyles =
    [ ( "list-style", "none" )
    , ( "padding", "0" )
    , ( "margin", "auto" )
    , ( "max-height", "200px" )
    , ( "overflow-y", "auto" )
    ]


itemStyles : List ( String, String )
itemStyles =
    [ ( "display", "block" )
    , ( "padding", "5px 10px" )
    , ( "border-bottom", "1px solid #DDD" )
    , ( "cursor", "pointer" )
    ]


inputStyles : List ( String, String )
inputStyles =
    [ ( "min-width", "120px" )
    , ( "color", "black" )
    , ( "position", "relative" )
    , ( "display", "block" )
    , ( "padding", "0.8em" )
    , ( "font-size", "12px" )
    ]
