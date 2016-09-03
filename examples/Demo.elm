module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import AccessibleExample
import SectionsExample


main : Program Never
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map AccessibleExample (AccessibleExample.subscriptions model.accessibleAutocomplete)
        , Sub.map SectionsExample (SectionsExample.subscriptions model.sectionsAutocomplete)
        ]


type alias Model =
    { accessibleAutocomplete : AccessibleExample.Model
    , sectionsAutocomplete : SectionsExample.Model
    }


init : Model
init =
    { accessibleAutocomplete = AccessibleExample.init
    , sectionsAutocomplete = SectionsExample.init
    }


type Msg
    = AccessibleExample AccessibleExample.Msg
    | SectionsExample SectionsExample.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AccessibleExample autoMsg ->
            { model | accessibleAutocomplete = fst <| AccessibleExample.update autoMsg model.accessibleAutocomplete } ! []

        SectionsExample autoMsg ->
            { model | sectionsAutocomplete = fst <| SectionsExample.update autoMsg model.sectionsAutocomplete } ! []


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ viewElmLink
        , viewApp model
        ]


viewElmLink : Html Msg
viewElmLink =
    a [ href "http://elm-lang.org/", value "_blank", class "elm-link" ]
        [ img
            [ class "elm-link-image"
            , src "http://elm-lang.org/assets/logo.svg"
            ]
            []
        ]


viewApp : Model -> Html Msg
viewApp model =
    div [ class "app" ]
        [ viewHeader model
        , viewExamples model
        , viewFooter
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "section header" ]
        [ h1 [ class "header-text" ] [ text "Elm Autocomplete" ]
        , viewLogo
        , p [ class "header-description" ] [ text "A reusable, navigable menu for all your text input needs." ]
        , a
            [ class "try-it-link"
            , href "https://github.com/thebritican/elm-autocomplete#installation"
            , target "_blank"
            , rel "noopenner noreferrer"
            ]
            [ text "Try it out!" ]
        ]


viewLogo : Html Msg
viewLogo =
    div [ class "logo" ]
        [ div [ class "green-part" ]
            []
        , div [ class "yellow triangle" ]
            []
        , div [ class "gray triangle" ]
            []
        , div [ class "blue triangle" ]
            []
        ]


viewExamples : Model -> Html Msg
viewExamples model =
    div [ class "section examples" ]
        [ h1 [] [ text "Examples" ]
        , viewSimpleExample model.accessibleAutocomplete
        , viewSectionsExample model.sectionsAutocomplete
        ]


viewSimpleExample : AccessibleExample.Model -> Html Msg
viewSimpleExample autocomplete =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ h2 [] [ text "Simple" ]
            , p [] [ text "A list of presidents" ]
            ]
        , Html.map AccessibleExample (AccessibleExample.view autocomplete)
        ]


viewSectionsExample : SectionsExample.Model -> Html Msg
viewSectionsExample autocomplete =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ h2 [] [ text "Sections" ]
            , p [] [ text "Presidents sectioned by century" ]
            ]
        , Html.map SectionsExample (SectionsExample.view autocomplete)
        ]


viewFooter : Html Msg
viewFooter =
    div [ class "section footer" ]
        [ p []
            [ text "Page design inspired by "
            , footerLink "http://react-autosuggest.js.org/" "React Autosuggest"
            ]
        , p []
            [ text "Created by "
            , footerLink "https://twitter.com/gregziegan" "Greg Ziegan"
            ]
        ]


footerLink : String -> String -> Html Msg
footerLink url text' =
    a
        [ href url
        , class "footer-link"
        , target "_blank"
        , rel "noopenner noreferrer"
        ]
        [ text text' ]
