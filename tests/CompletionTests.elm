module CompletionTests exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-make CompletionTests.elm --output=elm.js
$ node elm.js

-}

import Assert
import Fuzz exposing (..)
import Test exposing (..)
import Test.Runner.Log
import Html.App
import Html
import Autocomplete.Autocomplete as Autocomplete
import Autocomplete.Config
import String


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \_ -> Html.text "Check the console for useful output!"
        }
        |> Test.Runner.Log.run testCompletion


checkWithinBoundedIndex : List String -> Int -> String -> ( String, String )
checkWithinBoundedIndex items index desiredItem =
    let
        remainingList =
            List.drop index items

        reversedList =
            List.reverse items

        getLastItem =
            case List.head reversedList of
                Just item ->
                    item

                Nothing ->
                    ""
    in
        case List.head remainingList of
            Just item ->
                ( item, desiredItem )

            Nothing ->
                ( getLastItem, desiredItem )


firstItem : String
firstItem =
    "elm"


initialItems : List String
initialItems =
    [ firstItem, "is", "fun" ]


newItems : List String
newItems =
    [ firstItem, "is", "fun", "and", "functional" ]


testCompletion : Test
testCompletion =
    describe "completes"
        [ describe "given list of strings"
            [ test "the first element is selected"
                <| \() ->
                    Autocomplete.init initialItems
                        |> Autocomplete.getSelectedItem
                        |> Assert.equal firstItem
            ]
        , describe "given an immediate complete message"
            [ fuzz (list string) "the current value is filled w/ first item"
                <| \items ->
                    Autocomplete.init items
                        |> Autocomplete.update Autocomplete.Complete
                        |> fst
                        |> Autocomplete.getCurrentValue
                        |> (\value ->
                                case List.head items of
                                    Just first ->
                                        Assert.equal first value

                                    Nothing ->
                                        Assert.equal "" value
                           )
            ]
        , describe "given a completion message and a selection index"
            [ fuzz2 (list string) int "the currently selected item replaces the current value"
                <| \items index ->
                    let
                        config =
                            Autocomplete.Config.defaultConfig
                                |> Autocomplete.Config.setMaxListSize (List.length items)

                        ( item, desiredItem ) =
                            Autocomplete.initWithConfig items config
                                |> Autocomplete.update (Autocomplete.ChangeSelection index)
                                |> fst
                                |> Autocomplete.update Autocomplete.Complete
                                |> fst
                                |> Autocomplete.getCurrentValue
                                |> checkWithinBoundedIndex items index
                    in
                        Assert.equal item desiredItem
            ]
        , describe "given a ShowMenu Msg"
            [ test "with a True argument"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init []
                                |> Autocomplete.update (Autocomplete.ShowMenu True)
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.true "showMenu value is set to True" model.showMenu
            , test "with a False argument"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init []
                                |> Autocomplete.update (Autocomplete.ShowMenu False)
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.false "showMenu value is set to False" model.showMenu
            ]
        , describe "given an UpdateItems Msg"
            [ test "with a list of strings, the items match the list of strings"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init initialItems
                                |> Autocomplete.update (Autocomplete.UpdateItems newItems)
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.equal newItems model.items
            , test "with a list of strings, the matches are updated correctly"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init initialItems
                                |> Autocomplete.update (Autocomplete.SetValue "e")
                                |> fst
                                |> Autocomplete.update (Autocomplete.UpdateItems newItems)

                        expectedMatches =
                            List.filter (\item -> String.startsWith "e" item) newItems
                                |> List.sort
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.equal expectedMatches model.matches
            ]
        , describe "given a ShowLoading Msg"
            [ test "with a True argument"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init []
                                |> Autocomplete.update (Autocomplete.SetLoading True)
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.true "isLoading value is set to True" model.isLoading
            , test "with a False argument"
                <| \() ->
                    let
                        ( autocomplete, _ ) =
                            Autocomplete.init []
                                |> Autocomplete.update (Autocomplete.SetLoading False)
                    in
                        case autocomplete of
                            Autocomplete.Autocomplete model ->
                                Assert.false "isLoading value is set to False" model.isLoading
            ]
        ]
