module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, char, float, int, list, map, string, tuple, unit)
import List.Extra
import RedBlackTrees exposing (..)
import Test exposing (..)


{-| Comparable values include numbers, characters, strings,
lists of comparable things, and tuples of comparable things.

This fuzzer chooses a single type or list of these comparisons.

-}
comparable : Fuzzer ( String, Int )
comparable =
    tuple ( constant "comparable", int )


{-| Helper to map the compabible values.
-}
constant c =
    map (always c) unit


suite : Test
suite =
    concat
        [ emptyTest
        , singletonTest
        , fromListTest
        , insertTest
        , deleteTest
        , searchTest
        , isMemberTest
        , sizeTest
        , blackHeightTest
        , heightTest
        , maximumTest
        , flattenTest
        , isValidTest
        , malformedTrees
        ]



--- Testing expected behaviour of trees generated from the libraries constructors.


emptyTest : Test
emptyTest =
    test "The empty costructor returns an Empty node" <|
        \_ ->
            empty
                |> Expect.equal Empty


singletonTest : Test
singletonTest =
    fuzz comparable "A singleton should always return a black node with empty children" <|
        \value ->
            singleton value
                |> Expect.equal (Node value Black Empty Empty)


fromListTest : Test
fromListTest =
    describe "All list constructions should be valid Red Black trees"
        [ fuzz (list int) "Checking lists of Ints" <|
            \list ->
                fromList list |> isValid |> Expect.equal True
        , fuzz (list float) "Checking lists of Floats" <|
            \list ->
                fromList list |> isValid |> Expect.equal True
        , fuzz (list char) "Checking lists of Chars" <|
            \list ->
                fromList list |> isValid |> Expect.equal True
        , fuzz (list string) "Checking lists of Strings" <|
            \list ->
                fromList list |> isValid |> Expect.equal True
        ]


insertTest : Test
insertTest =
    describe "Inserting values should yield valid trees"
        [ test "Inserting 1 into a tree with a node of 8" <|
            \_ ->
                singleton 8 |> insert 1 |> Expect.equal (Node 8 Black (Node 1 Red Empty Empty) Empty)
        , fuzz2 (list comparable) comparable "Inserting comparable into a tree built by a list of comparables" <|
            \list value ->
                fromList list |> insert value |> isValid |> Expect.equal True
        , fuzz2 (list <| tuple ( string, int )) (tuple ( string, int )) "Inserting a (string, int) tuple into a tree built of the same" <|
            \list value ->
                fromList list |> insert value |> isValid |> Expect.equal True
        ]


deleteTest : Test
deleteTest =
    describe "Deleting a value should yield a valid tree" <|
        [ test "Removing 1 from a tree build by a [1,2,3,4] list" <|
            \_ ->
                fromList [ 1, 2, 3, 4 ] |> delete 1 |> Expect.equal (Node 3 Black (Node 2 Black Empty Empty) (Node 4 Black Empty Empty))
        , test "Removing 2 from a tree build by a [1,2,3,4] list" <|
            \_ ->
                fromList [ 1, 2, 3, 4 ] |> delete 2 |> Expect.equal (Node 3 Black (Node 1 Black Empty Empty) (Node 4 Black Empty Empty))
        , test "Removing 3 from a tree build by a [1,2,3,4] list" <|
            \_ ->
                fromList [ 1, 2, 3, 4 ] |> delete 3 |> Expect.equal (Node 2 Black (Node 1 Black Empty Empty) (Node 4 Black Empty Empty))
        , test "Removing 4 from a tree build by a [1,2,3,4] list" <|
            \_ ->
                fromList [ 1, 2, 3, 4 ] |> delete 4 |> Expect.equal (Node 2 Black (Node 1 Black Empty Empty) (Node 3 Black Empty Empty))
        , fuzz2 (list comparable) comparable "Requesting deletion of a comparable from a tree built by a list of comparables" <|
            \list value ->
                fromList list |> delete value |> isValid |> Expect.equal True
        , fuzz2 (list comparable) comparable "Deleting a comparable from a tree built by a list of comparables" <|
            \list value ->
                fromList list |> insert value |> delete value |> isValid |> Expect.equal True
        , fuzz2 (list <| tuple ( int, float )) (tuple ( int, float )) "Requesting deletion of a (int, float) tuple from a tree built of the same" <|
            \list value ->
                fromList list |> delete value |> isValid |> Expect.equal True
        , fuzz2 (list <| tuple ( int, float )) (tuple ( int, float )) "Deleting a (int, float) tuple from a tree built of the same" <|
            \list value ->
                fromList list |> insert value |> delete value |> isValid |> Expect.equal True
        ]



--- Testing utilities


searchTest : Test
searchTest =
    let
        tree =
            fromList [ 2, 5, 6, 7, 1, 8, 4, 3 ]
    in
    describe "Search orders should recover predictable lists"
        [ test "Pre-order" <|
            \_ ->
                tree |> preOrder |> Expect.equal [ 5, 3, 2, 1, 4, 7, 6, 8 ]
        , test "In-order" <|
            \_ ->
                tree |> inOrder |> Expect.equal [ 1, 2, 3, 4, 5, 6, 7, 8 ]
        , test "Post-order" <|
            \_ ->
                tree |> postOrder |> Expect.equal [ 1, 2, 4, 3, 6, 8, 7, 5 ]
        , test "Level-order" <|
            \_ ->
                tree |> levelOrder |> Expect.equal [ 5, 3, 7, 2, 4, 6, 8, 1 ]
        , fuzz (list comparable) "In-order search should be a sorted, unique list" <|
            \list ->
                fromList list |> inOrder |> Expect.equal (List.sort <| List.Extra.unique list)
        ]


isMemberTest : Test
isMemberTest =
    describe "Testing membership"
        [ test "Expected in tree" <|
            \_ ->
                fromList [ 1, 2, 3 ] |> isMember 3 |> Expect.equal True
        , test "Not expected in tree" <|
            \_ ->
                fromList [ 1, 2, 3 ] |> isMember 4 |> Expect.equal False
        ]


sizeTest : Test
sizeTest =
    describe "Testing expected tree size"
        [ test "Empty tree" <|
            \_ ->
                empty |> size |> Expect.equal 0
        , test "Singleton tree" <|
            \_ ->
                singleton 1 |> size |> Expect.equal 1
        , fuzz (list comparable) "Trees from arbitrarily sized lists" <|
            \list ->
                fromList list |> size |> Expect.equal (List.length <| List.Extra.unique list)
        ]


blackHeightTest : Test
blackHeightTest =
    describe "Testing expected black heights"
        [ test "Expecting 0" <|
            \_ ->
                empty |> blackHeight |> Expect.equal (Just 0)
        , test "Expecting 1" <|
            \_ ->
                singleton 2 |> blackHeight |> Expect.equal (Just 1)
        , test "Expecting 2" <|
            \_ ->
                fromList [ 2, 7, 4, 9, 1, 3, 18, 10 ] |> blackHeight |> Expect.equal (Just 2)
        ]


heightTest : Test
heightTest =
    describe "Testing expected tree height properties"
        [ test "Empty tree" <|
            \_ ->
                empty |> height |> Expect.equal 0
        , test "Singleton tree" <|
            \_ ->
                singleton 1 |> height |> Expect.equal 1
        , test "Known height" <|
            \_ ->
                fromList [ 8, 24, 17, 32, 9, 1, 12, 7 ] |> height |> Expect.equal 4
        , fuzz (list comparable) "Trees from arbitrarily sized lists, expecting height is at most twice the black height" <|
            \list ->
                let
                    tree =
                        fromList list

                    bh =
                        Maybe.withDefault 0 (blackHeight tree)
                in
                tree |> height |> Expect.atMost (2 * bh)
        ]


maximumTest : Test
maximumTest =
    describe "Testing maximum value"
        [ test "Empty" <|
            \_ ->
                empty |> maximum |> Expect.equal Nothing
        , test "Known max" <|
            \_ ->
                fromList [ 1, 9, 2, 7 ] |> maximum |> Expect.equal (Just 9)
        , fuzz (list comparable) "Trees from arbitrarily sized lists" <|
            \list ->
                fromList list |> maximum |> Expect.equal (List.maximum list)
        ]


flattenTest : Test
flattenTest =
    test "Testing tree flattening" <|
        \_ ->
            fromList [ 8, 1, 2, 6, 29, 42, 7, 22, 18, 36 ] |> flatten |> Expect.equal [ 1, 2, 6, 7, 8, 18, 22, 29, 36, 42 ]



--- Validity testing


isValidTest : Test
isValidTest =
    describe "Identify valid Red Black trees"
        [ test "Generated tree is valid" <|
            \_ ->
                fromList [ 1, 2, 3, 4 ] |> isValid |> Expect.equal True
        , test "Unordered tree fails binary search order test" <|
            \_ ->
                Node 2 Black (Node 7 Black Empty Empty) (Node 3 Black Empty (Node 4 Red Empty Empty)) |> isValid |> Expect.equal False
        , test "Red root node fails test" <|
            \_ ->
                Node 5 Red Empty Empty |> isValid |> Expect.equal False
        , test "Red-red failure" <|
            \_ ->
                Node 8 Red (Node 1 Red Empty Empty) Empty |> isValid |> Expect.equal False
        , test "Black height failure" <|
            \_ ->
                Node 2 Black (Node 1 Red Empty Empty) (Node 3 Black Empty (Node 4 Red Empty Empty)) |> isValid |> Expect.equal False
        ]



--- Malformed tree handling. These failures cannot come from the libraries generator functions, but may occur if trees are modified in userspace.


malformedTrees : Test
malformedTrees =
    describe "Edge case handling for when malformed trees are introduced"
        [ test "DoubleBlack colour outside of delete" <|
            \_ ->
                Node 5 DoubleBlack Empty Empty |> insert 1 |> isValid |> Expect.equal True
        , test "NegativeBlack colour outside of delete" <|
            \_ ->
                Node 5 NegativeBlack Empty Empty |> insert 1 |> isValid |> Expect.equal True
        , test "DoubleEmpty outside of delete" <|
            -- This one is a known frailty. See Libbum/elm-redblacktrees#7
            \_ ->
                Node 5 Black DoubleEmpty Empty |> insert 1 |> isValid |> Expect.equal False
        ]
