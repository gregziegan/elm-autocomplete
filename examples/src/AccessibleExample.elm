module AccessibleExample exposing (..)

import Browser
import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Menu
import String
import Task


main : Program () Model Msg
main =
    Browser.embed
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SetAutoState Menu.subscription


type alias Model =
    { people : List Person
    , autoState : Menu.State
    , howManyToShow : Int
    , query : String
    , selectedPerson : Maybe Person
    , showMenu : Bool
    }


init : Model
init =
    { people = presidents
    , autoState = Menu.empty
    , howManyToShow = 5
    , query = ""
    , selectedPerson = Nothing
    , showMenu = False
    }


type Msg
    = SetQuery String
    | SetAutoState Menu.Msg
    | Wrap Bool
    | Reset
    | HandleEscape
    | SelectPersonKeyboard String
    | SelectPersonMouse String
    | PreviewPerson String
    | OnFocus
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            let
                showMenu =
                    not (List.isEmpty (acceptablePeople newQuery model.people))
            in
            ( { model
                | query = newQuery
                , showMenu = showMenu
                , selectedPerson = Nothing
              }
            , Cmd.none
            )

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Menu.update updateConfig
                        autoMsg
                        model.howManyToShow
                        model.autoState
                        (acceptablePeople model.query model.people)

                newModel =
                    { model | autoState = newState }
            in
            maybeMsg
                |> Maybe.map (\updateMsg -> update updateMsg newModel)
                |> Maybe.withDefault ( newModel, Cmd.none )

        HandleEscape ->
            let
                validOptions =
                    not (List.isEmpty (acceptablePeople model.query model.people))

                handleEscape =
                    if validOptions then
                        model
                            |> removeSelection
                            |> resetMenu

                    else
                        resetInput model

                escapedModel =
                    case model.selectedPerson of
                        Just person ->
                            if model.query == person.name then
                                resetInput model

                            else
                                handleEscape

                        Nothing ->
                            handleEscape
            in
            ( escapedModel, Cmd.none )

        Wrap toTop ->
            case model.selectedPerson of
                Just person ->
                    update Reset model

                Nothing ->
                    if toTop then
                        ( { model
                            | autoState =
                                Menu.resetToLastItem updateConfig
                                    (acceptablePeople model.query model.people)
                                    model.howManyToShow
                                    model.autoState
                            , selectedPerson =
                                acceptablePeople model.query model.people
                                    |> List.take model.howManyToShow
                                    |> List.reverse
                                    |> List.head
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | autoState =
                                Menu.resetToFirstItem updateConfig
                                    (acceptablePeople model.query model.people)
                                    model.howManyToShow
                                    model.autoState
                            , selectedPerson =
                                acceptablePeople model.query model.people
                                    |> List.take model.howManyToShow
                                    |> List.head
                          }
                        , Cmd.none
                        )

        Reset ->
            ( { model
                | autoState = Menu.reset updateConfig model.autoState
                , selectedPerson = Nothing
              }
            , Cmd.none
            )

        SelectPersonKeyboard id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
            ( newModel, Cmd.none )

        SelectPersonMouse id ->
            let
                newModel =
                    setQuery model id
                        |> resetMenu
            in
            ( newModel, Task.attempt (\_ -> NoOp) (Browser.focus "president-input") )

        PreviewPerson id ->
            ( { model
                | selectedPerson =
                    Just (getPersonAtId model.people id)
              }
            , Cmd.none
            )

        OnFocus ->
            ( model
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


resetInput model =
    { model | query = "" }
        |> removeSelection
        |> resetMenu


removeSelection model =
    { model | selectedPerson = Nothing }


getPersonAtId people id =
    List.filter (\person -> person.name == id) people
        |> List.head
        |> Maybe.withDefault (Person "" 0 "" "")


setQuery model id =
    { model
        | query = .name (getPersonAtId model.people id)
        , selectedPerson = Just (getPersonAtId model.people id)
    }


resetMenu model =
    { model
        | autoState = Menu.empty
        , showMenu = False
    }


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "true"

        False ->
            "false"


view : Model -> Html.Html Msg
view model =
    let
        upDownEscDecoderHelper : Int -> Decode.Decoder Msg
        upDownEscDecoderHelper code =
            if code == 38 || code == 40 then
                Decode.succeed NoOp

            else if code == 27 then
                Decode.succeed HandleEscape

            else
                Decode.fail "not handling that key"

        upDownEscDecoder : Decode.Decoder ( Msg, Bool )
        upDownEscDecoder =
            Events.keyCode
                |> Decode.andThen upDownEscDecoderHelper
                |> Decode.map (\msg -> ( msg, True ))

        menu =
            if model.showMenu then
                [ viewMenu model ]

            else
                []

        query =
            model.selectedPerson
                |> Maybe.map .name
                |> Maybe.withDefault model.query

        activeDescendant attributes =
            model.selectedPerson
                |> Maybe.map .name
                |> Maybe.map (Attrs.attribute "aria-activedescendant")
                |> Maybe.map (\attribute -> attribute :: attributes)
                |> Maybe.withDefault attributes
    in
    Html.div []
        (List.append
            [ Html.input
                (activeDescendant
                    [ Events.onInput SetQuery
                    , Events.onFocus OnFocus
                    , Events.preventDefaultOn "keydown" upDownEscDecoder
                    , Attrs.value query
                    , Attrs.id "president-input"
                    , Attrs.class "autocomplete-input"
                    , Attrs.autocomplete False
                    , Attrs.attribute "aria-owns" "list-of-presidents"
                    , Attrs.attribute "aria-expanded" (boolToString model.showMenu)
                    , Attrs.attribute "aria-haspopup" (boolToString model.showMenu)
                    , Attrs.attribute "role" "combobox"
                    , Attrs.attribute "aria-autocomplete" "list"
                    ]
                )
                []
            ]
            menu
        )


acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
    let
        lowerQuery =
            String.toLower query
    in
    List.filter (String.contains lowerQuery << String.toLower << .name) people


viewMenu : Model -> Html.Html Msg
viewMenu model =
    Html.div [ Attrs.class "autocomplete-menu" ]
        [ Html.map SetAutoState <|
            Menu.view viewConfig
                model.howManyToShow
                model.autoState
                (acceptablePeople model.query model.people)
        ]


updateConfig : Menu.UpdateConfig Msg Person
updateConfig =
    Menu.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Maybe.map PreviewPerson maybeId

                else if code == 13 then
                    Maybe.map SelectPersonKeyboard maybeId

                else
                    Just Reset
        , onTooLow = Just (Wrap False)
        , onTooHigh = Just (Wrap True)
        , onMouseEnter = \id -> Just (PreviewPerson id)
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just (SelectPersonMouse id)
        , separateSelections = False
        }


viewConfig : Menu.ViewConfig Person
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ Attrs.classList
                    [ ( "autocomplete-item", True )
                    , ( "key-selected", keySelected || mouseSelected )
                    ]
                , Attrs.id person.name
                ]
            , children = [ Html.text person.name ]
            }
    in
    Menu.viewConfig
        { toId = .name
        , ul = [ Attrs.class "autocomplete-list" ]
        , li = customizedLi
        }



-- PEOPLE


type alias Person =
    { name : String
    , year : Int
    , city : String
    , state : String
    }


presidents : List Person
presidents =
    [ Person "George Washington" 1732 "Westmoreland County" "Virginia"
    , Person "John Adams" 1735 "Braintree" "Massachusetts"
    , Person "Thomas Jefferson" 1743 "Shadwell" "Virginia"
    , Person "James Madison" 1751 "Port Conway" "Virginia"
    , Person "James Monroe" 1758 "Monroe Hall" "Virginia"
    , Person "Andrew Jackson" 1767 "Waxhaws Region" "South/North Carolina"
    , Person "John Quincy Adams" 1767 "Braintree" "Massachusetts"
    , Person "William Henry Harrison" 1773 "Charles City County" "Virginia"
    , Person "Martin Van Buren" 1782 "Kinderhook" "New York"
    , Person "Zachary Taylor" 1784 "Barboursville" "Virginia"
    , Person "John Tyler" 1790 "Charles City County" "Virginia"
    , Person "James Buchanan" 1791 "Cove Gap" "Pennsylvania"
    , Person "James K. Polk" 1795 "Pineville" "North Carolina"
    , Person "Millard Fillmore" 1800 "Summerhill" "New York"
    , Person "Franklin Pierce" 1804 "Hillsborough" "New Hampshire"
    , Person "Andrew Johnson" 1808 "Raleigh" "North Carolina"
    , Person "Abraham Lincoln" 1809 "Sinking spring" "Kentucky"
    , Person "Ulysses S. Grant" 1822 "Point Pleasant" "Ohio"
    , Person "Rutherford B. Hayes" 1822 "Delaware" "Ohio"
    , Person "Chester A. Arthur" 1829 "Fairfield" "Vermont"
    , Person "James A. Garfield" 1831 "Moreland Hills" "Ohio"
    , Person "Benjamin Harrison" 1833 "North Bend" "Ohio"
    , Person "Grover Cleveland" 1837 "Caldwell" "New Jersey"
    , Person "William McKinley" 1843 "Niles" "Ohio"
    , Person "Woodrow Wilson" 1856 "Staunton" "Virginia"
    , Person "William Howard Taft" 1857 "Cincinnati" "Ohio"
    , Person "Theodore Roosevelt" 1858 "New York City" "New York"
    , Person "Warren G. Harding" 1865 "Blooming Grove" "Ohio"
    , Person "Calvin Coolidge" 1872 "Plymouth" "Vermont"
    , Person "Herbert Hoover" 1874 "West Branch" "Iowa"
    , Person "Franklin D. Roosevelt" 1882 "Hyde Park" "New York"
    , Person "Harry S. Truman" 1884 "Lamar" "Missouri"
    , Person "Dwight D. Eisenhower" 1890 "Denison" "Texas"
    , Person "Lyndon B. Johnson" 1908 "Stonewall" "Texas"
    , Person "Ronald Reagan" 1911 "Tampico" "Illinois"
    , Person "Richard M. Nixon" 1913 "Yorba Linda" "California"
    , Person "Gerald R. Ford" 1913 "Omaha" "Nebraska"
    , Person "John F. Kennedy" 1917 "Brookline" "Massachusetts"
    , Person "George H. W. Bush" 1924 "Milton" "Massachusetts"
    , Person "Jimmy Carter" 1924 "Plains" "Georgia"
    , Person "George W. Bush" 1946 "New Haven" "Connecticut"
    , Person "Bill Clinton" 1946 "Hope" "Arkansas"
    , Person "Barack Obama" 1961 "Honolulu" "Hawaii"
    ]
