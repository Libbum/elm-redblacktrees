module RedBlackTrees exposing
    ( RedBlackTree(..), Colour(..)
    , empty, singleton, fromList, insert, delete
    , preOrder, inOrder, postOrder
    , levelOrder
    , isMember, size, blackHeight, height, maximum, flatten
    , isValid
    )

{-| Red Black Trees are self-balancing binary seach trees which add one bit of
memory to the standard BST (usually denoted by the colours red and black). With
this additional tracking information; the worst case search, insert and delete
time complexity drops to O(log N) [from O(N) in the BST case].


# Types

@docs RedBlackTree, Colour


# Building and Modifying

@docs empty, singleton, fromList, insert, delete


# Searching


## Depth First

@docs preOrder, inOrder, postOrder


## Breadth First

@docs levelOrder


# Utilities

@docs isMember, size, blackHeight, height, maximum, flatten


# Validation

@docs isValid

-}

import Fifo exposing (Fifo)



--- Types


{-| Trees can be comprised of either empty leaves or nodes containing a value,
a represesentative colour and two child branches.

The `DoubleEmpty` value is only used when deleting, so isn't needed when constructing trees manually.

-}
type RedBlackTree comparable
    = Empty
    | DoubleEmpty
    | Node comparable Colour (RedBlackTree comparable) (RedBlackTree comparable)


{-| Since this is a red black tree representation, we ignore the green brown convention.

The additional `DoubleBlack` and `NegativeBlack` colours are required for deletion
purposes, thus aren't usually used when building trees.

-}
type Colour
    = Red
    | Black
    | DoubleBlack
    | NegativeBlack



--- Building


{-| An empty tree for ease of use when constructing trees.

    empty
    --> Empty

-}
empty : RedBlackTree comparable
empty =
    Empty


{-| A tree with a single value inserted into it. Since this
is a single node tree, it's colour is black by definition.

    singleton 5
    --> Node 5 Black Empty Empty

-}
singleton : comparable -> RedBlackTree comparable
singleton value =
    Node value Black Empty Empty


{-| Generate a Red Black representation of a list.

    fromList [ 2, 7, 8, 3, 9, 1, 5, 10 ]
    --> Node 7 Black (Node 3 Red (Node 2 Black (Node 1 Red Empty Empty) Empty) (Node 5 Black Empty Empty)) (Node 9 Red (Node 8 Black Empty Empty) (Node 10 Black Empty Empty))

-}
fromList : List comparable -> RedBlackTree comparable
fromList =
    List.foldl insert Empty


{-| Adds a new value to the tree. Since this may cause on of the four
red black constraints to be broken, there may be a need to recolour nodes
or rebalance the tree.

    singleton 8 |> insert 1
    --> Node 8 Black (Node 1 Red Empty Empty) Empty

-}
insert : comparable -> RedBlackTree comparable -> RedBlackTree comparable
insert x tree =
    blacken (ins x tree)


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

        DoubleEmpty ->
            tree


{-| A red-red violation can occur in any of the four initial scenarios.
If any one of them occur, the solution is the same regardelss of the arrangement
of the violation. Red-red violations will be propogated up the tree until
they no longer appear.

The lower six cases are verification cases when deleting nodes. There
are four red-red like violations that propegate the same, although
they are blacker. Finally, two cases that handle negative blacks.

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

        Node z DoubleBlack (Node y Red (Node x Red a b) c) d ->
            Node y Black (Node x Black a b) (Node z Black c d)

        Node z DoubleBlack (Node x Red a (Node y Red b c)) d ->
            Node y Black (Node x Black a b) (Node z Black c d)

        Node x DoubleBlack a (Node z Red (Node y Red b c) d) ->
            Node y Black (Node x Black a b) (Node z Black c d)

        Node x DoubleBlack a (Node y Red b (Node z Red c d)) ->
            Node y Black (Node x Black a b) (Node z Black c d)

        Node x DoubleBlack a (Node z NegativeBlack (Node y Black b c) d) ->
            case d of
                Node val Black l r ->
                    Node y Black (Node x Black a b) (balance <| Node z Black c (redden d))

                _ ->
                    tree

        Node z DoubleBlack (Node x NegativeBlack a (Node y Black b c)) d ->
            case a of
                Node val Black l r ->
                    Node y Black (balance <| Node x Black (redden a) b) (Node z Black c d)

                _ ->
                    tree

        _ ->
            tree


{-| Remove a node from the tree. Most of the time this is a straightforward
matter, except for when a black node with no children is removed. This
ultimately changes the (`blackHeight`)[#blackHeight] and thus the entire
tree must be rebalanced and recoloured.

    tree = fromList [1,2,3,4]
    --> Node 2 Black (Node 1 Black Empty Empty) (Node 3 Black Empty (Node 4 Red Empty Empty))

    delete 1 tree
    --> Node 3 Black (Node 2 Black Empty Empty) (Node 4 Black Empty Empty)

    delete 2 tree
    --> Node 3 Black (Node 1 Black Empty Empty) (Node 4 Black Empty Empty)

    delete 3 tree
    --> Node 2 Black (Node 1 Black Empty Empty) (Node 4 Black Empty Empty)

    delete 4 tree
    --> Node 2 Black (Node 1 Black Empty Empty) (Node 3 Black Empty Empty)

-}
delete : comparable -> RedBlackTree comparable -> RedBlackTree comparable
delete x tree =
    blacken (del x tree)


{-| Handles the recursive deletion. First, we bubble up any `DoubleBlack`
and then call `remove` to alter the tree where the deletion must occur.
-}
del : comparable -> RedBlackTree comparable -> RedBlackTree comparable
del x tree =
    case tree of
        Node y colour left right ->
            if x < y then
                bubble y colour (del x left) right

            else if x > y then
                bubble y colour left (del x right)

            else
                remove tree

        _ ->
            Empty


{-| Deletion helper. Performs the deletion operation once all DoubleBlacks
have been bubbled out.
-}
remove : RedBlackTree comparable -> RedBlackTree comparable
remove tree =
    case tree of
        Node x Red Empty Empty ->
            Empty

        Node x Black Empty Empty ->
            DoubleEmpty

        Node x Black Empty (Node y Red left right) ->
            Node y Black left right

        Node x Black (Node y Red left right) Empty ->
            Node y Black left right

        Node x colour left right ->
            case maximum left of
                Just maxLeft ->
                    bubble maxLeft colour (removeMax left) right

                Nothing ->
                    tree

        _ ->
            Empty


{-| Remove the largest value in a tree.
-}
removeMax : RedBlackTree comparable -> RedBlackTree comparable
removeMax tree =
    case tree of
        Node x colour left Empty ->
            remove tree

        Node x colour left right ->
            bubble x colour left (removeMax right)

        _ ->
            tree


{-| Check if tree has DoubleBlack nodes.
-}
isDoubleBlack : RedBlackTree comparable -> Bool
isDoubleBlack tree =
    case tree of
        DoubleEmpty ->
            True

        Node x DoubleBlack left right ->
            True

        _ ->
            False


{-| Colour a node red.
-}
redden : RedBlackTree comparable -> RedBlackTree comparable
redden tree =
    case tree of
        Node x colour left right ->
            Node x Red left right

        _ ->
            tree


{-| Colour a node black.
-}
blacken : RedBlackTree comparable -> RedBlackTree comparable
blacken tree =
    case tree of
        Node x colour left right ->
            Node x Black left right

        _ ->
            Empty


{-| Blacken a given colour.
-}
blacker : Colour -> Colour
blacker colour =
    case colour of
        NegativeBlack ->
            Red

        Red ->
            Black

        _ ->
            DoubleBlack


{-| Blacken the entire tree from this point.
-}
blackerTree : RedBlackTree comparable -> RedBlackTree comparable
blackerTree tree =
    case tree of
        Node x colour left right ->
            Node x (blacker colour) left right

        Empty ->
            DoubleEmpty

        _ ->
            tree


{-| Redden a given colour.
-}
redder : Colour -> Colour
redder colour =
    case colour of
        Black ->
            Red

        DoubleBlack ->
            Black

        _ ->
            NegativeBlack


{-| Redden the entire tree from this point.
-}
redderTree : RedBlackTree comparable -> RedBlackTree comparable
redderTree tree =
    case tree of
        Node x colour left right ->
            Node x (redder colour) left right

        DoubleEmpty ->
            Empty

        _ ->
            tree


{-| This helper function "bubbles" double-blackness upward.
-}
bubble : comparable -> Colour -> RedBlackTree comparable -> RedBlackTree comparable -> RedBlackTree comparable
bubble x colour left right =
    if isDoubleBlack left || isDoubleBlack right then
        balance <| Node x (blacker colour) (redderTree left) (redderTree right)

    else
        balance <| Node x colour left right



--- Search


{-| A pre-order depth-first search: start at the root, then
traverse the left branch followed by the right branch.

    fromList [ 2, 5, 6, 7, 1, 8, 4, 3 ] |> preOrder
    --> [ 5, 3, 2, 1, 4, 7, 6, 8 ]

-}
preOrder : RedBlackTree comparable -> List comparable
preOrder tree =
    case tree of
        Node x colour left right ->
            [ x ] ++ preOrder left ++ preOrder right

        _ ->
            []


{-| An in-order depth-first search: traverse the left branch,
add the root, then finish with the right branch. This ordering
is sorted by convention.

    fromList [ 2, 5, 6, 7, 1, 8, 4, 3 ] |> inOrder
    --> [ 1, 2, 3, 4, 5, 6, 7, 8 ]

-}
inOrder : RedBlackTree comparable -> List comparable
inOrder tree =
    case tree of
        Node x colour left right ->
            inOrder left ++ [ x ] ++ inOrder right

        _ ->
            []


{-| A post-order depth-first search: traverse the left branch followed by
the right branch and finishing with the root.

    fromList [ 2, 5, 6, 7, 1, 8, 4, 3 ] |> postOrder
    --> [ 1, 2, 4, 3, 6, 8, 7, 5 ]

-}
postOrder : RedBlackTree comparable -> List comparable
postOrder tree =
    case tree of
        Node x colour left right ->
            postOrder left ++ postOrder right ++ [ x ]

        _ ->
            []


{-| A breadth-first search traversing the tree in level order,
starting from the root and travering down.

    fromList [ 2, 5, 6, 7, 1, 8, 4, 3 ] |> levelOrder
    --> [ 5, 3, 7, 2, 4, 6, 8, 1 ]

-}
levelOrder : RedBlackTree comparable -> List comparable
levelOrder tree =
    case tree of
        Node x colour left right ->
            breadthFirst (Fifo.insert tree Fifo.empty)

        _ ->
            []


{-| Helper function that actually completes the breadth-first seach
called by `levelOrder`.
-}
breadthFirst : Fifo (RedBlackTree comparable) -> List comparable
breadthFirst queue =
    let
        ( maybe, queuedValue ) =
            Fifo.remove queue
    in
    case maybe of
        Nothing ->
            []

        Just node ->
            case node of
                Node x colour left right ->
                    [ x ] ++ breadthFirst (Fifo.insert right (Fifo.insert left queuedValue))

                _ ->
                    breadthFirst queuedValue



--- Utilities


{-| Check if a value currently exists within in a tree.

    fromList [ 1, 2, 3 ] |> isMember 72
    --> False

-}
isMember : comparable -> RedBlackTree comparable -> Bool
isMember x tree =
    case tree of
        Node y colour left right ->
            if x == y then
                True

            else if x < y then
                isMember x left

            else
                isMember x right

        _ ->
            False


{-| Count the number of elements in the tree.

    fromList [ 3, 8, 16 ] |> size
    --> 3

-}
size : RedBlackTree comparable -> Int
size tree =
    case tree of
        Node x colour left right ->
            1 + size left + size right

        _ ->
            0


{-| Every path from the root to the leaves of a red black tree must contain
the same number of black nodes. The `blackHeight` is the value of this path length.
Notably, this is also the shortest path from root to leaf.

    fromList [ 2, 7, 4, 9, 1, 3, 18, 10 ] |> blackHeight
    --> Just 2

Calling `blackHeight` on a valid red black tree will return a count, but if
the tree is not correctly balanced, this function will return `Nothing`.

-}
blackHeight : RedBlackTree comparable -> Maybe Int
blackHeight tree =
    case tree of
        Empty ->
            Just 0

        DoubleEmpty ->
            Nothing

        Node x colour left right ->
            let
                nodeCount =
                    case colour of
                        Red ->
                            Just 0

                        Black ->
                            Just 1

                        _ ->
                            Nothing
            in
            case ( blackHeight left, blackHeight right ) of
                ( Just leftCount, Just rightCount ) ->
                    if leftCount == rightCount then
                        Maybe.map2 (+) nodeCount (Just leftCount)

                    else
                        Nothing

                _ ->
                    Nothing


{-| Calculate the height of the tree.

    fromList [ 8, 24, 17, 32, 9, 1, 12, 7 ] |> height
    --> 4

The longest path from the root to a leaf is at most twice the length of
the shortest path.

    height tree <= 2 * (Maybe.withDefault 0 <| blackHeight tree)
    --> True

-}
height : RedBlackTree comparable -> Int
height tree =
    case tree of
        Node x colour left right ->
            1 + max (height left) (height right)

        _ ->
            0


{-| Finds largest element in tree. Returns `Nothing`
if tree is `Empty`.

    fromList [1,9,2,7] |> maximum
    --> Just 9

-}
maximum : RedBlackTree comparable -> Maybe comparable
maximum tree =
    case tree of
        Node x colour left Empty ->
            Just x

        Node x colour left right ->
            maximum right

        _ ->
            Nothing


{-| Generate a list of values contained in the tree. Since
Red Black trees are an extention of Binary Search Trees, the
resultant list will be sorted. Colour is ignored in this operation.

    tree = fromList [ 8, 1, 2, 6, 29, 42, 7, 22, 18, 36 ]
    --> Node 7 Black (Node 2 Black (Node 1 Black Empty Empty) (Node 6 Black Empty Empty)) (Node 29 Black (Node 18 Red (Node 8 Black Empty Empty) (Node 22 Black Empty Empty)) (Node 42 Black (Node 36 Red Empty Empty) Empty))

    flatten tree
    --> [ 1, 2, 6, 7, 8, 18, 22, 29, 36, 42 ]

-}
flatten : RedBlackTree comparable -> List comparable
flatten tree =
    case tree of
        Node value colour left right ->
            List.concat [ flatten left, [ value ], flatten right ]

        _ ->
            []



--- Validation


{-| Verifies that a given tree is a valid red black tree by checking

1.  It satisfies the binary search order property
2.  The root node is coloured `Black`
3.  No red node has a child node that is also red
4.  Every path from the root to a leaf contains the same number of black nodes

```
fromList [ 1, 2, 3, 4 ] |> isValid
--> True
```

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

        DoubleEmpty ->
            False

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


{-| Helper for `binarySearchOrder` which (possibly) grabs the value of the root node.
-}
root : RedBlackTree comparable -> Maybe comparable
root tree =
    case tree of
        Node x colour left right ->
            Just x

        _ ->
            Nothing


{-| Here we check if there are any red nodes that have red children.
If so, the red-red constraint has been violated. It may be possible to
fix this by balancing the tree.
-}
noRedRed : RedBlackTree comparable -> Bool
noRedRed tree =
    case tree of
        Empty ->
            True

        DoubleEmpty ->
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

        DoubleEmpty ->
            False

        Node x Black left right ->
            True

        Node x colour left right ->
            False
