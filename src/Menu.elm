module Menu
    exposing
        ( HtmlDetails
        , KeySelected
        , MouseSelected
        , Msg
        , SectionConfig
        , SectionNode
        , State
        , UpdateConfig
        , ViewConfig
        , ViewWithSectionsConfig
        , current
        , empty
        , reset
        , resetToFirstItem
        , resetToLastItem
        , sectionConfig
        , subscription
        , update
        , updateConfig
        , view
        , viewConfig
        , viewWithSections
        , viewWithSectionsConfig
        )

{-| This library helps you create a menu.
Your data is stored separately; keep it in whatever shape makes the most sense for your application.
A menu has a lot of uses: form input, mentions, search, etc.

I have (hopefully!) given the users of this library a large amount of customizability.

I recommend looking at the [`examples`][examples] before diving into the API or source code.

[examples]: https://github.com/ContaSystemer/elm-menu/tree/master/examples


# View

@docs view


# Update

@docs update, subscription


# Configuration

@docs viewConfig, updateConfig


# State

@docs State, current, empty, reset, resetToFirstItem, resetToLastItem, KeySelected, MouseSelected


# Definitions

@docs Msg, ViewConfig, UpdateConfig, HtmlDetails


# Sections

Sections require a separate view and configuration since another type of data must be
provided: sections.

**Note:** Section data can have any shape: your static configuration will
just tell the menu how to grab an ID for a section and its related data.


# View

@docs viewWithSections


# Configuration

@docs sectionConfig, viewWithSectionsConfig


# Definitions

@docs SectionNode, SectionConfig, ViewWithSectionsConfig

-}

import Char
import Html
import Menu.Internal as Internal


{-| Tracks keyboard and mouse selection within the menu.
-}
type State
    = State Internal.State


{-| True if the element has been selected via keyboard navigation.
-}
type alias KeySelected =
    Bool


{-| True if the element has been selected via mouse hover.
-}
type alias MouseSelected =
    Bool


{-| Current State.
-}
current : State -> ( Maybe String, Maybe String )
current (State state) =
    Internal.current state


{-| A State with nothing selected.
-}
empty : State
empty =
    State Internal.empty


{-| Reset the keyboard navigation but leave the mouse state alone.
Convenient when the two selections are represented separately.
-}
reset : UpdateConfig msg data -> State -> State
reset (UpdateConfig config) (State state) =
    State (Internal.reset config state)


{-| Like `reset` but defaults to a keyboard selection of the first item.
-}
resetToFirstItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToFirstItem (UpdateConfig config) data howManyToShow (State state) =
    State (Internal.resetToFirstItem config data howManyToShow state)


{-| Like `reset` but defaults to a keyboard selection of the last item.
-}
resetToLastItem : UpdateConfig msg data -> List data -> Int -> State -> State
resetToLastItem (UpdateConfig config) data howManyToShow (State state) =
    State (Internal.resetToLastItem config data howManyToShow state)



-- UPDATE


{-| A message type for the menu to update.
-}
type Msg
    = Msg Internal.Msg


{-| Configuration for updates
-}
type UpdateConfig msg data
    = UpdateConfig (Internal.UpdateConfig msg data)


{-| Use this function to update the menu's `State`.
Provide the same data as your view.
The `Int` argument is how many results you would like to show.
-}
update : UpdateConfig msg data -> Msg -> Int -> State -> List data -> ( State, Maybe msg )
update (UpdateConfig config) (Msg msg) howManyToShow (State state) data =
    let
        ( newState, maybeMsg ) =
            Internal.update config msg howManyToShow state data
    in
    ( State newState, maybeMsg )


{-| Create the configuration for your `update` function (`UpdateConfig`).
Say we have a `List Person` that we want to show as a series of options.
We would create an `UpdateConfig` like so:

    import Menu

    updateConfig : Menu.UpdateConfig Msg Person
    updateConfig =
        Menu.updateConfig
            { toId = .name
            , onKeyDown =
                \code maybeId ->
                    if code == 38 || code == 40 then
                        Nothing

                    else if code == 13 then
                        Maybe.map SelectPerson maybeId

                    else
                        Just Reset
            , onTooLow = Nothing
            , onTooHigh = Nothing
            , onMouseEnter = \_ -> Nothing
            , onMouseLeave = \_ -> Nothing
            , onMouseClick = \id -> Just (SelectPerson id)
            , separateSelections = False
            }

You provide the following information in your menu configuration:

  - `toId` &mdash; turn a `Person` into a unique ID.
  - `ul` &mdash; specify any non-behavioral attributes you'd like for the list menu.
  - `li` &mdash; specify any non-behavioral attributes and children for a list item: both selection states are provided.

-}
updateConfig :
    { toId : data -> String
    , onKeyDown : Int -> Maybe String -> Maybe msg
    , onTooLow : Maybe msg
    , onTooHigh : Maybe msg
    , onMouseEnter : String -> Maybe msg
    , onMouseLeave : String -> Maybe msg
    , onMouseClick : String -> Maybe msg
    , separateSelections : Bool
    }
    -> UpdateConfig msg data
updateConfig config =
    UpdateConfig (Internal.updateConfig config)


{-| Add this to your `program`'s subscriptions so the menu will respond to keyboard input.
-}
subscription : Sub Msg
subscription =
    Sub.map Msg Internal.subscription


{-| Take a list of `data` and turn it into a menu.
The `ViewConfig` argument is the configuration for the menu view.
`ViewConfig` describes the HTML we want to show for each item and the list.
The `Int` argument is how many results you would like to show.
The `State` argument describes what is selected via mouse and keyboard.

**Note:** The `State` and `List data` should live in your Model.
The `ViewConfig` for the menu belongs in your view code.
`ViewConfig` should never exist in your model.
Describe any potential menu configurations statically.
This pattern has been inspired by [Elm Sortable Table](http://package.elm-lang.org/packages/evancz/elm-sortable-table/latest).

-}
view : ViewConfig data -> Int -> State -> List data -> Html.Html Msg
view (ViewConfig config) howManyToShow (State state) data =
    Html.map Msg (Internal.view config howManyToShow state data)


{-| Presents a menu with sections.
You can follow the same instructions as described for `view`, providing a more advanced configuration and different data shape.
`ViewWithSectionsConfig` sets up your menu to handle sectioned data.
The sectioned data becomes the new data argument for `viewWithSections`.
-}
viewWithSections : ViewWithSectionsConfig data sectionData -> Int -> State -> List sectionData -> Html.Html Msg
viewWithSections (ViewWithSectionsConfig config) howManyToShow (State state) sections =
    Html.map Msg (Internal.viewWithSections config howManyToShow state sections)


{-| HTML lists require `li` tags as children, so we allow you to specify everything about `li` HTML node except the nodeType.
-}
type alias HtmlDetails msg =
    { attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    }


{-| Configuration for your menu, describing your menu and its items.

**Note:** Your `ViewConfig` should never be held in your model. It should only appear in view code.

-}
type ViewConfig data
    = ViewConfig (Internal.ViewConfig data)


{-| Create the configuration for your `view` function (`ViewConfig`).
Say we have a `List Person` that we want to show as a series of options.
We would create a `ViewConfig` like so:

    import Menu

    viewConfig : Menu.ViewConfig Person
    viewConfig =
        let
            customizedLi keySelected mouseSelected person =
                { attributes =
                    [ classList
                        [ ( "menu-item", True )
                        , ( "key-selected", keySelected )
                        , ( "mouse-selected", mouseSelected )
                        ]
                    ]
                , children = [ Html.text person.name ]
                }
        in
        Menu.viewConfig
            { toId = .name
            , ul = [ class "menu-list" ]
            , li = customizedLi
            }

You provide the following information in your menu configuration:

  - `toId` &mdash; turn a `Person` into a unique ID. This lets us use
    [`Html.Keyed`][keyed] under the hood to make sorting faster.
  - `ul` &mdash; specify any non-behavioral attributes you'd like for the list menu.
  - `li` &mdash; specify any non-behavioral attributes and children for a list item: both selection states are provided.
    See the [examples] to get a better understanding!

[keyed]: http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Keyed
[examples]: https://github.com/ContaSystemer/elm-menu/tree/master/examples

-}
viewConfig :
    { toId : data -> String
    , ul : List (Html.Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    }
    -> ViewConfig data
viewConfig config =
    ViewConfig (Internal.viewConfig config)


{-| Configuration for your menu, describing your menu, its sections, and its items.

**Note:** This should never live in your model.

-}
type ViewWithSectionsConfig data sectionData
    = ViewWithSectionsConfig (Internal.ViewWithSectionsConfig data sectionData)


{-| The same configuration as viewConfig, but provide a section configuration as well.
-}
viewWithSectionsConfig :
    { toId : data -> String
    , ul : List (Html.Attribute Never)
    , li : KeySelected -> MouseSelected -> data -> HtmlDetails Never
    , section : SectionConfig data sectionData
    }
    -> ViewWithSectionsConfig data sectionData
viewWithSectionsConfig { toId, ul, li, section } =
    let
        (SectionConfig internalSection) =
            section
    in
    ViewWithSectionsConfig <|
        Internal.viewWithSectionsConfig
            { toId = toId
            , ul = ul
            , li = li
            , section = internalSection
            }


{-| The configuration for a section of the menu.

**Note:** This should never live in your model.

-}
type SectionConfig data sectionData
    = SectionConfig (Internal.SectionConfig data sectionData)


{-| Describe everything about a Section HTML node.
-}
type alias SectionNode msg =
    { nodeType : String
    , attributes : List (Html.Attribute msg)
    , children : List (Html.Html msg)
    }


{-| Create the `SectionConfig` for your `view` function.
Say we have a `List Century` that we want to show as a series of sections.
We would create a `SectionConfig` like so:

    type alias Century =
      { title : String
      , people : List Person
      }

    import Menu

    sectionConfig : Menu.SectionConfig Person Century
    sectionConfig =
        Menu.sectionConfig
            { toId = .title
            , getData = .people
            , ul = [ class "menu-section-list" ]
            , li =
                \section ->
                    { nodeType = "div"
                    , attributes = [ class "menu-section-item" ]
                    , children =
                        [ div [ class "menu-section-box" ]
                            [ strong [ class "menu-section-text" ] [ text section.title ]
                            ]
                        ]
                    }
            }

You provide the following information in your menu configuration:

  - `toId` &mdash; turn a `Century` into a unique ID.
  - `getData` &mdash; extract the data from `Century`, in this case: `List Person`.
  - `ul` &mdash; specify any non-behavioral attributes you'd like for the section list.
  - `li` &mdash; specify any non-behavioral attributes and children for a section.

-}
sectionConfig :
    { toId : sectionData -> String
    , getData : sectionData -> List data
    , ul : List (Html.Attribute Never)
    , li : sectionData -> SectionNode Never
    }
    -> SectionConfig data sectionData
sectionConfig section =
    SectionConfig (Internal.sectionConfig section)
