module Autocomplete
    exposing
        ( State
        , KeySelected
        , MouseSelected
        , empty
        , reset
        , resetToFirstItem
        , Msg
        , UpdateConfig
        , updateConfig
        , update
        , view
        , viewWithSections
        , ViewConfig
        , ViewWithSectionsConfig
        , HtmlDetails
        , viewConfig
        , viewWithSectionsConfig
        , SectionConfig
        , SectionNode
        , sectionConfig
        , subscription
        )

{-| A customizable Autocomplete component.


# Definition
@docs State, KeySelected, MouseSelected, empty, reset, resetToFirstItem, ViewConfig, ViewWithSectionsConfig, HtmlDetails, viewConfig

# Update
@docs Msg, update, UpdateConfig, updateConfig, subscription

# View
@docs view, viewWithSections, SectionConfig, viewWithSectionsConfig, sectionConfig, SectionNode

-}

import Autocomplete.Autocomplete as Internal
import Html exposing (..)
import Html.App as Html
import Char exposing (KeyCode)


{-| The Autocomplete model.
-}
type State
    = State Internal.State


{-| -}
type alias KeySelected =
    Bool


{-| -}
type alias MouseSelected =
    Bool


{-| -}
empty : State
empty =
    State Internal.empty


{-| -}
reset : State -> State
reset (State state) =
    State <| Internal.reset state


{-| -}
resetToFirstItem : List data -> (data -> String) -> State -> State
resetToFirstItem data toId (State state) =
    State <| Internal.resetToFirstItem data toId state



-- UPDATE


{-| -}
type Msg
    = Msg Internal.Msg


{-| Configuration for updates
-}
type UpdateConfig msg data
    = UpdateConfig (Internal.UpdateConfig msg data)


{-| -}
update : UpdateConfig msg data -> Msg -> State -> List data -> Int -> ( State, Maybe msg )
update (UpdateConfig config) (Msg msg) (State state) data howManyToShow =
    let
        ( newState, maybeMsg ) =
            Internal.update config msg state data howManyToShow
    in
        ( State newState, maybeMsg )


{-| -}
updateConfig :
    { onKeyDown : KeyCode -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , toId : data -> String
    , separateSelections : Bool
    }
    -> UpdateConfig msg data
updateConfig config =
    UpdateConfig <| Internal.updateConfig config


{-| Add this to your `program`s subscriptions to animate the spinner.
-}
subscription : Sub Msg
subscription =
    Sub.map Msg Internal.subscription


{-| -}
view : ViewConfig data -> Int -> State -> List data -> Html Msg
view (ViewConfig config) howManyToShow (State state) data =
    Html.map Msg <| Internal.view config howManyToShow state data


{-| -}
viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State -> List sectionData -> Html Msg
viewWithSections (ViewWithSectionsConfig config) howManyToShow (State state) sections =
    Html.map Msg <| Internal.viewWithSections config howManyToShow state sections


{-| -}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| -}
type ViewConfig data
    = ViewConfig (Internal.ViewConfig data)


{-| Create the configuration for your `view` function (`ViewConfig`).
Say we have a `List Person` that we want to show as a series of options.
We would create a `ViewConfig` like this:
    import Autocomplete
    config : Table.Config Person Msg
    config =
      Table.viewConfig
        { toId = .name
        , ul = [ class "autocomplete-list" ]
        , li = customizedLi
        }

    customizedLi :
        Autocomplete.KeySelected
        -> Autocomplete.MouseSelected
        -> Person
        -> Autocomplete.HtmlDetails Never
    customizedLi keySelected mouseSelected person =
        if keySelected then
            { attributes = [ class "autocomplete-key-item" ]
            , children = [ Html.text person.name ]
            }
        else if mouseSelected then
            { attributes = [ class "autocomplete-mouse-item" ]
            , children = [ Html.text person.name ]
            }
        else
            { attributes = [ class "autocomplete-item" ]
            , children = [ Html.text person.name ]
            }
You provide the following information in your autocomplete configuration:
  - `toId` &mdash; turn a `Person` into a unique ID. This lets us use
  [`Html.Keyed`][keyed] under the hood to make sorting faster.
  - `ul` &mdash; specify any non-behavioral attributes you'd like for the list menu.
  - `li` &mdash; specify any non-behavioral attributes and children for a list item: both selection states are provided
See the [examples][] to get a better feel for this!
[keyed]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Keyed
[examples]: https://github.com/thebritican/elm-autocomplete/tree/master/examples
-}
viewConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }
    -> ViewConfig data
viewConfig config =
    ViewConfig <| Internal.viewConfig config


{-| -}
type ViewWithSectionsConfig data sectionData
    = ViewWithSectionsConfig (Internal.ViewWithSectionsConfig data sectionData)


{-| -}
viewWithSectionsConfig :
    { toId : data -> String
    , ul : List (Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig config =
    ViewWithSectionsConfig
        <| case config.section of
            SectionConfig section ->
                Internal.viewWithSectionsConfig { config | section = section }


{-| A section of the menu
-}
type SectionConfig data sectionData
    = SectionConfig (Internal.SectionConfig data sectionData)


{-| -}
type alias SectionNode msg =
    { nodeType : String
    , attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Create the `SectionConfig` for your `view` function.
Say we have a `List Century` that we want to show as a series of sections.
We would create a `SectionConfig` like this:

    type alias Century =
      { title : String
      , people : List Person
      }

    import Autocomplete
    sectionConfig : Autocomplete.SectionConfig Person Century
    sectionConfig =
        Autocomplete.sectionConfig
            { toId = .title
            , getData = .people
            , ul = [ class "autocomplete-section-list" ]
            , li =
                \section ->
                    { nodeType = "div"
                    , attributes = [ class "autocomplete-section-item" ]
                    , children =
                        [ div [ class "autocomplete-section-box" ]
                            [ strong [ class "autocomplete-section-text" ] [ text section.title ]
                            ]
                        ]
                    }
            }



You provide the following information in your autocomplete configuration:
  - `toId` &mdash; turn a `Century` into a unique ID. This lets us use
  [`Html.Keyed`][keyed] under the hood to make sorting faster.
  - `getData` &mdash; extract the data from `Century`, in this case: `List Person`
  - `ul` &mdash; specify any non-behavioral attributes you'd like for the section list.
  - `li` &mdash; specify any non-behavioral attributes and children for a section
See the [examples][] to get a better feel for this!
[keyed]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Keyed
[examples]: https://github.com/thebritican/elm-autocomplete/tree/master/examples
-}
sectionConfig :
    { toId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Attribute Never)
    , li : sectionData -> SectionNode Never
    }
    -> SectionConfig data sectionData
sectionConfig section =
    SectionConfig <| Internal.sectionConfig section
