module Tests exposing (..)

import Test exposing (..)
import Expect


-- import Html
-- import Html.Attributes exposing (..)

import String


-- updateConfig : Autocomplete.UpdateConfig msg Person
-- updateConfig =
--     Autocomplete.updateConfig
--         { onKeyDown =
--             \code maybeId -> Nothing
--         , onTooLow = Nothing
--         , onTooHigh = Nothing
--         , onMouseEnter = \_ -> Nothing
--         , onMouseLeave = \_ -> Nothing
--         , onMouseClick = \id -> Nothing
--         , toId = .name
--         }
-- viewConfig : Autocomplete.ViewConfig Person
-- viewConfig =
--     Autocomplete.viewConfig
--         { toId = .name
--         , ul = [ class "autocomplete-list" ]
--         , li = myLi
--         , input = [ class "autocomplete-input", placeholder "Search by name" ]
--         }
--
--
-- myLi :
--     Autocomplete.KeySelected
--     -> Autocomplete.MouseSelected
--     -> Person
--     -> Autocomplete.HtmlDetails Never
-- myLi keySelected mouseSelected person =
--     if keySelected then
--         { attributes = [ class "autocomplete-key-item" ]
--         , children = [ Html.text person.name ]
--         }
--     else if mouseSelected then
--         { attributes = [ class "autocomplete-mouse-item" ]
--         , children = [ Html.text person.name ]
--         }
--     else
--         { attributes = [ class "autocomplete-item" ]
--         , children = [ Html.text person.name ]
--         }
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



-- all : Test
-- all =
--     describe "completes"
--         [ describe "given list of data"
--             [ test "the first element is selected on down key press"
--                 <| \() ->
--                     let
--                         ( state, maybeMsg ) =
--                             Autocomplete.update updateConfig (Autocomplete.KeyDown 40) Autocomplete.empty presidents 5
--                     in
--                         Expect.equal (Maybe.withDefault "" state.key) "George Washington"
--             ]
--         ]


all : Test
all =
    describe "A Test Suite"
        [ test "Addition"
            <| \() ->
                Expect.equal (3 + 7) 10
        , test "String.left"
            <| \() ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
