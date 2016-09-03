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
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "header" ]
        [ h1 [ class "header-text" ] [ text "Elm Autocomplete" ]
        , viewLogo
        , p [ class "header-description" ] [ text "A reusable, navigable menu for all your text input needs." ]
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
    div [ class "examples" ]
        [ h1 [] [ text "Examples" ]
        , Html.map AccessibleExample (AccessibleExample.view model.accessibleAutocomplete)
        , Html.map SectionsExample (SectionsExample.view model.sectionsAutocomplete)
        ]
