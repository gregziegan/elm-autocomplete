module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple
import AccessibleExample
import SectionsExample
import Svg exposing (path)
import Svg.Attributes exposing (d, fill, viewBox)


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentFocus of
        Simple ->
            Sub.map AccessibleExample (AccessibleExample.subscriptions model.accessibleAutocomplete)

        Sections ->
            Sub.map SectionsExample (SectionsExample.subscriptions model.sectionsAutocomplete)

        None ->
            Sub.none


type alias Model =
    { accessibleAutocomplete : AccessibleExample.Model
    , sectionsAutocomplete : SectionsExample.Model
    , currentFocus : Focused
    }


type Focused
    = Simple
    | Sections
    | None


init : Model
init =
    { accessibleAutocomplete = AccessibleExample.init
    , sectionsAutocomplete = SectionsExample.init
    , currentFocus = None
    }


type Msg
    = AccessibleExample AccessibleExample.Msg
    | SectionsExample SectionsExample.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                AccessibleExample autoMsg ->
                    let
                        toggleFocus autoMsg model =
                            case autoMsg of
                                AccessibleExample.OnFocus ->
                                    { model | currentFocus = Simple }

                                _ ->
                                    model
                    in
                        { model | accessibleAutocomplete = Tuple.first <| AccessibleExample.update autoMsg model.accessibleAutocomplete }
                            |> toggleFocus autoMsg

                SectionsExample autoMsg ->
                    let
                        toggleFocus autoMsg model =
                            case autoMsg of
                                SectionsExample.OnFocus ->
                                    { model | currentFocus = Sections }

                                _ ->
                                    model
                    in
                        { model | sectionsAutocomplete = Tuple.first <| SectionsExample.update autoMsg model.sectionsAutocomplete }
                            |> toggleFocus autoMsg
    in
        ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ viewElmLink
        , viewForkMe
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


viewForkMe : Html Msg
viewForkMe =
    a [ attribute "aria-label" "View source on Github", class "github-corner", href "https://github.com/thebritican/elm-autocomplete" ]
        [ Svg.svg [ attribute "aria-hidden" "true", Svg.Attributes.height "80", Svg.Attributes.style "fill:#70B7FD; color:#fff; position: absolute; top: 0; border: 0; right: 0;", viewBox "0 0 250 250", Svg.Attributes.width "80" ]
            [ path [ d "M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z" ]
                []
            , path [ Svg.Attributes.class "octo-arm", d "M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2", fill "currentColor", Svg.Attributes.style "transform-origin: 130px 106px;" ]
                []
            , path [ Svg.Attributes.class "octo-body", d "M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z", fill "currentColor" ]
                []
            ]
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
        [ h1 [ class "section-title" ] [ text "Elm Autocomplete" ]
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
        [ h1 [ class "section-title" ] [ text "Examples" ]
        , viewSimpleExample model.accessibleAutocomplete
        , viewSectionsExample model.sectionsAutocomplete
        ]


viewSimpleExample : AccessibleExample.Model -> Html Msg
viewSimpleExample autocomplete =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ h1 [ class "example-title" ] [ text "Simple" ]
            , p [] [ text "A list of presidents" ]
            ]
        , div [ class "example-autocomplete" ]
            [ Html.map AccessibleExample (AccessibleExample.view autocomplete)
            ]
        ]


viewSectionsExample : SectionsExample.Model -> Html Msg
viewSectionsExample autocomplete =
    div [ class "example" ]
        [ div [ class "example-info" ]
            [ h1 [ class "example-title" ] [ text "Sections" ]
            , p [] [ text "Presidents grouped by birth century" ]
            ]
        , div [ class "example-autocomplete" ] [ Html.map SectionsExample (SectionsExample.view autocomplete) ]
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
footerLink url text_ =
    a
        [ href url
        , class "footer-link"
        , target "_blank"
        , rel "noopenner noreferrer"
        ]
        [ text text_ ]
