port module Main exposing (..)

import Autocomplete
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port caretPosition : (CaretPosition -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SetAutoState Autocomplete.subscription
        , caretPosition SetCaretPosition
        ]


type alias Model =
    { people : List Person
    , autoState : Autocomplete.State
    , howManyToShow : Int
    , value : String
    , position : Position
    , query : String
    , showMenu : Bool
    , caretPos : CaretPosition
    }


type alias CaretPosition =
    { top : Int
    , left : Int
    }


type alias Position =
    Int


init : Model
init =
    { people = presidents
    , autoState = Autocomplete.empty
    , howManyToShow = 5
    , value = ""
    , position = 0
    , query = ""
    , showMenu = False
    , caretPos = { top = 0, left = 0 }
    }


type Msg
    = SetValue String
    | SetAutoState Autocomplete.Msg
    | Reset Bool
    | SelectPerson String
    | SetCaretPosition CaretPosition
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetValue value ->
            let
                stringToPos =
                    String.slice 0 model.position model.value

                mention =
                    String.dropLeft model.position model.value

                query =
                    if String.endsWith "@" stringToPos then
                        mention
                    else
                        value

                showMenu =
                    not << List.isEmpty <| (acceptablePeople query model.people)
            in
                { model | value = value, showMenu = showMenu, query = query } ! []

        SetAutoState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg model.howManyToShow model.autoState (acceptablePeople model.query model.people)

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        Reset toTop ->
            { model
                | autoState =
                    if toTop then
                        Autocomplete.resetToFirstItem updateConfig (acceptablePeople model.query model.people) model.howManyToShow model.autoState
                    else
                        Autocomplete.resetToLastItem updateConfig (acceptablePeople model.query model.people) model.howManyToShow model.autoState
            }
                ! []

        SelectPerson id ->
            let
                meh =
                    List.filter (\person -> person.name == id) model.people
            in
                { model
                    | query =
                        List.filter (\person -> person.name == id) model.people
                            |> List.head
                            |> Maybe.withDefault (Person "" 0 "" "")
                            |> .name
                    , autoState = Autocomplete.empty
                    , showMenu = False
                }
                    ! []

        SetCaretPosition pos ->
            { model | caretPos = pos } ! []

        NoOp ->
            model ! []


view : Model -> Html Msg
view model =
    let
        options =
            { preventDefault = True, stopPropagation = False }

        dec =
            (Json.map
                (\code ->
                    if code == 38 || code == 40 then
                        Ok NoOp
                    else
                        Err "not handling that key"
                )
                keyCode
            )
                |> Json.andThen fromResult

        fromResult : Result String a -> Json.Decoder a
        fromResult result =
            case result of
                Ok val ->
                    Json.succeed val

                Err reason ->
                    Json.fail reason

        menu =
            if model.showMenu then
                [ viewMenu model ]
            else
                []
    in
        div []
            (List.append
                [ h1 [] [ text "U.S. Presidents" ]
                , viewEditor model
                ]
                menu
            )


viewEditor : Model -> Html Msg
viewEditor model =
    textarea
        [ onInput SetValue
        , value model.value
        ]
        []


acceptablePeople : String -> List Person -> List Person
acceptablePeople query people =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .name) people


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "autocomplete-menu" ]
        [ Html.map SetAutoState (Autocomplete.view viewConfig model.howManyToShow model.autoState (acceptablePeople model.query model.people)) ]


updateConfig : Autocomplete.UpdateConfig Msg Person
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 38 || code == 40 then
                    Nothing
                else if code == 13 then
                    Maybe.map SelectPerson maybeId
                else
                    Just <| Reset False
        , onTooLow = Just <| Reset True
        , onTooHigh = Just <| Reset False
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPerson id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig Person
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes =
                [ classList [ ( "autocomplete-item", True ), ( "key-selected", keySelected || mouseSelected ) ]
                , id person.name
                ]
            , children = [ Html.text person.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul = [ class "autocomplete-list" ]
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
