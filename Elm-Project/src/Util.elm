module Util exposing (groupBy, maximumBy, maybeToList, minimumBy, zipFilter)

{-| Module containing utility functions
-}


{-| Description for minimumBy

    minimumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    minimumBy .x [] --> Nothing

    minimumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 23

-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy comp list =
    let
        helper lst min=
            case lst of
                [] -> Just min
                x::xs -> if (comp x) < (comp min) then helper xs x else helper xs min
    in
        case list of
            [] -> Nothing
            x::xs -> helper xs x
                
{-| Description for maximumBy

    maximumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    maximumBy .x [] --> Nothing

    maximumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 16

-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy comp list =
    let
        helper lst max=
            case lst of
                [] -> Just max
                x::xs -> if (comp x) > (comp max) then helper xs x else helper xs max
    in
        case list of
            [] -> Nothing
            x::xs -> helper xs x

{-| Group a list

    groupBy .x [ { x = 1 } ] --> [(1, [{x = 1}])]

    groupBy (modBy 10) [ 11, 12, 21, 22 ] --> [(1, [11, 21]), (2, [12, 22])]

    groupBy identity [] --> []

-}
groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy op list =
    let 
        addTuples list1 tuple =
            let
                helper1 lst acc ok=
                    case lst of
                    [] -> if ((List.isEmpty acc) || ok) then List.reverse (tuple::acc) else List.reverse acc
                    x::xs -> if (\(u,v) -> u) x == (\(u,v) -> u) tuple then helper1 xs (((\(u,v) -> u) x,(\(u,v) -> v) x ++ (\(u,v) -> v) tuple)::acc) False
                            else helper1 xs (x::acc) ok
            in
                helper1 list1 [] True
           
        makeTuples op1 list2 =
            List.map (\x -> (op1 x, [x])) list2
            
        helper2 lst acc =
            case lst of
            [] -> acc
            x::xs ->if List.isEmpty acc then helper2 xs (x::acc) else helper2 xs (addTuples acc x)
    in
        helper2 (makeTuples op list) []
    
            

{-| Transforms a Maybe into a List with one element for Just, and an empty list for Nothing

    maybeToList (Just 1) --> [1]

    maybeToList Nothing --> []

-}
maybeToList : Maybe a -> List a
maybeToList maybeSomething =
    case maybeSomething of
        Nothing -> []
        Just something -> [something]


{-| Filters a list based on a list of bools

    zipFilter [ True, True ] [ 1, 2 ] --> [1, 2]

    zipFilter [ False, False ] [ 1, 2 ] --> []

    zipFilter [ True, False, True, False ] [ 1, 2, 3, 4 ] --> [1, 3]

-}
zipFilter : List Bool -> List a -> List a
zipFilter listB list =
    let
        zip lx ly =
            case (lx, ly) of
                (x::xs, y::ys) -> (x, y)::(zip xs ys)
                _ -> []
    in
        List.filterMap (\(x,y) -> if x then Just y else Nothing) (zip listB list)