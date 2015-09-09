module Join where
{-| A Join is a set of data that changes at discrete moments. A join tracks items that are entering the set, exiting the
set, or updating from one value to another. Elements are tracked across updates not by equality but by index or a key
function.

You can use this library to transition between views of data. For example, some data points might transition away as
they're filtered out, while others will smoothly change size or position. The enter-update-exit pattern has been very
successful in [D3](http://d3js.org/). Here, we've removed the DOM entirely; BYO rendering pipeline.

## The Join Type
@docs Join

## Create
@docs create, empty

## Update
@docs update, updateBy

## Edit
@docs edit

-}

import Dict

{-| The `Join` type consists of three lists. The enter list contains elements that were not part of the set prior to the
last update. The exit list contains elements that were part of the set before the last update but are not anymore. The
update list tracks the old and new values (in that order) of elements the have stayed in the set.

You'll typically create joins with the functions below, but access the lists using normal Elm record accessors.
-}
type alias Join a =
    { enter : List a
    , update : List (a,a) -- old, new
    , exit : List a
    }

{-| Create a join, with the provided elements entering and no elements updating or exiting.
-}
create : List a -> Join a
create xs = Join xs [] []

{-| Create a join with no elements. -}
empty : Join a
empty = Join [] [] []

{-| Update a join with new elements, which are matched to the old elements by index.
-}
update : List a -> Join a -> Join a
update new {enter, update} =
    let existing = List.map snd update ++ enter
        update' = List.map2 (,) existing new
        enter' = List.drop (List.length existing) new
        exit' = List.drop (List.length new) existing
    in Join enter' update' exit'

{-| Update a join with new elements, which are matched to the old elements by a key function. The key function should
return a *unique* value for all items in the new list, and in the old enter and update lists together. Looking up an id
in a record works well.
-}
updateBy : (a -> comparable) -> List a -> Join a -> Join a
updateBy key new {enter, update} =
    let existing = List.map snd update ++ enter
        fromKeys = List.foldl (\d -> Dict.insert (key d) d) Dict.empty
        newDict = fromKeys new
        extDict = fromKeys existing
        commonKeys = Dict.intersect newDict extDict |> Dict.keys
        unsafeGet k d = Dict.get k d |> (\(Just x) -> x)
        update' = List.map (\k -> (unsafeGet k extDict, unsafeGet k newDict)) commonKeys
        enter' = Dict.diff newDict extDict |> Dict.values
        exit' = Dict.diff extDict newDict |> Dict.values
    in Join enter' update' exit'

{-| Edit the update and enter lists together. Entering items will have `Nothing` while updating values will have `Just`
their old value. This distinction is used to split the list once you're done with it, so typically you'll treat the
first element as read-only. You can use this function to map over or sort all values currently in the set.
-}
edit : (List (Maybe a, a) -> List (Maybe a, a)) -> Join a -> Join a
edit f {enter, update, exit} =
    let enter' = List.map (\d -> (Nothing, d)) enter
        update' = List.map (\(d1, d2) -> (Just d1, d2)) update
        combined = f <| enter' ++ update'
        isNothing m = case m of -- not in core :(
            Nothing -> True
            _ -> False
        (enter'', update'') = List.partition (fst>>isNothing) combined
    in Join (List.map snd enter'') (List.map (\(Just d1,d2) -> (d1,d2)) update'') exit
