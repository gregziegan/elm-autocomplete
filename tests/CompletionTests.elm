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


testCompletion : Test
testCompletion =
    describe "completes"
        [ describe "given list of strings"
            [ test "the first element is selected"
                <| \() ->
                    Autocomplete.init [ "elm", "is", "functional" ]
                        |> Autocomplete.getSelectedItem
                        |> Assert.equal "elm"
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
                                    Just firstItem ->
                                        Assert.equal firstItem value

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
        ]
