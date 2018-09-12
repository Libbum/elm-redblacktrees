module RedBlackTrees exposing
    ( RedBlackTree, Colour(..)
    , empty, fromList, insert
    , isMember, size, blackHeight, height
    , isValid
    )

{-| Red Black Trees are self-balancing binary seach trees which add one bit of
memory to the standard BST (usually denoted by the colours red and black). With
this additional tracking information; the worst case search, insert and delete
time complexity drops to O(log N) [from O(N) in the BST case].


# Types

@docs RedBlackTree, Colour


# Building

@docs empty, fromList, insert


# Utilities

@docs isMember, size, blackHeight, height


# Validation

@docs isValid

-}

--- Types


{-| Trees can be comprised of either empty leaves or nodes containing a value,
a represesentative colour and two child branches.
-}
type RedBlackTree comparable
    = Empty
    | Node comparable Colour (RedBlackTree comparable) (RedBlackTree comparable)


{-| Since this is a red black tree representation, we ignore the green brown convention.
-}
type Colour
    = Red
    | Black



--- Building


{-| An empty tree for ease of use when constructing trees.
-}
empty : RedBlackTree comparable
empty =
    Empty


{-| Generate a Red Black representation of a list.
-}
fromList : List comparable -> RedBlackTree comparable
fromList =
    List.foldl insert Empty


{-| Adds a new value to the tree. Since this may cause on of the four
red black constraints to be broken, there may be a need to recolour nodes
or rebalance the tree.
-}
insert : comparable -> RedBlackTree comparable -> RedBlackTree comparable
insert x tree =
    let
        balanced =
            ins x tree
    in
    case balanced of
        Empty ->
            Node x Black Empty Empty

        Node y colour left right ->
            Node y Black left right



--- Utilities


{-| Check if a value currently exists within in a tree.
-}
isMember : comparable -> RedBlackTree comparable -> Bool
isMember x tree =
    case tree of
        Empty ->
            False

        Node y colour left right ->
            if x == y then
                True

            else if x < y then
                isMember x left

            else
                isMember x right


{-| Count the number of elements in the tree.
-}
size : RedBlackTree comparable -> Int
size tree =
    case tree of
        Empty ->
            0

        Node x colour left right ->
            1 + size left + size right


{-| Every path from the root to the leaves of a red black tree must contain
the same number of black nodes. The `blackHeight` is the value of this path length.
Notably, this is also the shortest path from root to leaf.
-}
blackHeight : RedBlackTree comparable -> Maybe Int
blackHeight tree =
    case tree of
        Empty ->
            Just 0

        Node x colour left right ->
            let
                nodeCount =
                    case colour of
                        Red ->
                            0

                        Black ->
                            1
            in
            case ( blackHeight left, blackHeight right ) of
                ( Just leftCount, Just rightCount ) ->
                    if leftCount == rightCount then
                        Just (nodeCount + leftCount)

                    else
                        Nothing

                _ ->
                    Nothing


{-| Calculate the height of the tree. The longest path from the root to a leaf
is at most twice the length of the shortest path.

    height tree <= 2 * (Maybe.withDefault 0 <| blackHeight tree) == True

-}
height : RedBlackTree comparable -> Int
height tree =
    case tree of
        Empty ->
            0

        Node x colour left right ->
            1 + max (height left) (height right)



--- Validation


{-| Verifies that a given tree is a valid red black tree by checking

1.  It satisfies the binary search order property
2.  The root node is coloured `Black`
3.  No red node has a child node that is also red
4.  Every path from the root to a leaf contains the same number of black nodes

-}
isValid : RedBlackTree comparable -> Bool
isValid tree =
    let
        okBlack =
            case blackHeight tree of
                Just x ->
                    True

                Nothing ->
                    False
    in
    binarySearchOrder tree && blackRoot tree && noRedRed tree && okBlack


{-| A red black tree is essentially an extention of the less complex binary search tree.
The BST arranges the smallest of values to the left of the tree, and largest to the right.
We test this condition is met on a per node basis here - this arrangement is not reqired for
an entire red black tree.
-}
binarySearchOrder : RedBlackTree comparable -> Bool
binarySearchOrder tree =
    case tree of
        Empty ->
            True

        Node x colour left right ->
            let
                checkLeft =
                    case root left of
                        Nothing ->
                            True

                        Just y ->
                            y < x

                checkRight =
                    case root right of
                        Nothing ->
                            True

                        Just y ->
                            x < y
            in
            checkLeft
                && checkRight
                && binarySearchOrder left
                && binarySearchOrder right


{-| Here we check if there are any red nodes that have red children.
If so, the red-red constraint has been violated. It may be possible to
fix this by balancing the tree.
-}
noRedRed : RedBlackTree comparable -> Bool
noRedRed tree =
    case tree of
        Empty ->
            True

        Node x Red (Node y Red l r) right ->
            False

        Node x Red left (Node y Red l r) ->
            False

        Node x colour left right ->
            List.all noRedRed [ left, right ]


{-| Simply verifies that the root node is coloured black.
-}
blackRoot : RedBlackTree comparable -> Bool
blackRoot tree =
    case tree of
        Empty ->
            True

        Node x Black left right ->
            True

        Node x Red left right ->
            False



--- Helpers


{-| A helper function for `insert`. Ultimately this does the insertion, but
since the algorithm adds a `Red` node by default, the `insert` function must force
the root node to be black to satisfy the red black constraints.
-}
ins : comparable -> RedBlackTree comparable -> RedBlackTree comparable
ins x tree =
    case tree of
        Empty ->
            Node x Red Empty Empty

        Node y colour left right ->
            if x == y then
                tree

            else if x < y then
                balance <| Node y colour (ins x left) right

            else
                balance <| Node y colour left (ins x right)


{-| A red-red violation can occur in any of these four possibilities.
If this occurs, the solution is the same regardelss of the arrangement
of the violation. Red-red violations will be propogated up the tree until
they no longer appear.
-}
balance : RedBlackTree comparable -> RedBlackTree comparable
balance tree =
    case tree of
        Node z Black (Node y Red (Node x Red a b) c) d ->
            Node y Red (Node x Black a b) (Node z Black c d)

        Node z Black (Node x Red a (Node y Red b c)) d ->
            Node y Red (Node x Black a b) (Node z Black c d)

        Node x Black a (Node z Red (Node y Red b c) d) ->
            Node y Red (Node x Black a b) (Node z Black c d)

        Node x Black a (Node y Red b (Node z Red c d)) ->
            Node y Red (Node x Black a b) (Node z Black c d)

        _ ->
            tree


{-| Helper for `binarySearchOrder` which (possibly) grabs the value of the root node.
-}
root : RedBlackTree comparable -> Maybe comparable
root tree =
    case tree of
        Empty ->
            Nothing

        Node x colour left right ->
            Just x
