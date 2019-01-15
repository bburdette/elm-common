module Toop exposing (T1(..), T10(..), T11(..), T12(..), T2(..), T3(..), T4(..), T5(..), T6(..), T7(..), T8(..), T9(..), andX, applyT1, applyT2, lh, rest, takeT1, takeT10, takeT11, takeT12, takeT2, takeT3, takeT4, takeT5, takeT6, takeT7, takeT8, takeT9, tuply)


type T1 a
    = T1 a


applyT1 : (a -> b) -> T1 a -> b
applyT1 y (T1 a) =
    y a


type T2 a b
    = T2 a b


applyT2 : (a -> b -> x) -> T2 a b -> x
applyT2 y (T2 a b) =
    y a b


type T3 a b c
    = T3 a b c


applyT3 : (a -> b -> c -> x) -> T3 a b c -> x
applyT3 y (T3 a b c) =
    y a b c


type T4 a b c d
    = T4 a b c d


applyT4 : (a -> b -> c -> d -> x) -> T4 a b c d -> x
applyT4 y (T4 a b c d) =
    y a b c d


type T5 a b c d e
    = T5 a b c d e


applyT5 : (a -> b -> c -> d -> e -> x) -> T5 a b c d e -> x
applyT5 y (T5 a b c d e) =
    y a b c d e


type T6 a b c d e f
    = T6 a b c d e f


applyT6 : (a -> b -> c -> d -> e -> f -> x) -> T6 a b c d e f -> x
applyT6 y (T6 a b c d e f) =
    y a b c d e f


type T7 a b c d e f g
    = T7 a b c d e f g


applyT7 : (a -> b -> c -> d -> e -> f -> g -> x) -> T7 a b c d e f g -> x
applyT7 y (T7 a b c d e f g) =
    y a b c d e f g


type T8 a b c d e f g h
    = T8 a b c d e f g h


applyT8 : (a -> b -> c -> d -> e -> f -> g -> h -> x) -> T8 a b c d e f g h -> x
applyT8 y (T8 a b c d e f g h) =
    y a b c d e f g h


type T9 a b c d e f g h i
    = T9 a b c d e f g h i


applyT9 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> x) -> T9 a b c d e f g h i -> x
applyT9 y (T9 a b c d e f g h i) =
    y a b c d e f g h i


type T10 a b c d e f g h i j
    = T10 a b c d e f g h i j


applyT10 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> x) -> T10 a b c d e f g h i j -> x
applyT10 y (T10 a b c d e f g h i j) =
    y a b c d e f g h i j


type T11 a b c d e f g h i j k
    = T11 a b c d e f g h i j k


applyT11 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> x) -> T11 a b c d e f g h i j k -> x
applyT11 y (T11 a b c d e f g h i j k) =
    y a b c d e f g h i j k


type T12 a b c d e f g h i j k l
    = T12 a b c d e f g h i j k l


applyT12 : (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> x) -> T12 a b c d e f g h i j k l -> x
applyT12 y (T12 a b c d e f g h i j k l) =
    y a b c d e f g h i j k l


rest l =
    List.drop 1 l


lh : List a -> Maybe ( a, List a )
lh la =
    case List.head la of
        Just a ->
            Just ( a, rest la )

        Nothing ->
            Nothing


andX : (a -> b) -> List a -> Maybe ( b, List a )
andX f l =
    lh l
        |> Maybe.map (\( v, ls ) -> ( f v, ls ))


tuply : (a -> b -> c) -> ( a, b ) -> c
tuply f ( a, b ) =
    f a b


takeT1 : List a -> Maybe (T1 a)
takeT1 la =
    Maybe.map Tuple.first <|
        andX T1 la


takeT2 : List a -> Maybe (T2 a a)
takeT2 l =
    Maybe.map Tuple.first
        (andX T2 l
            |> Maybe.andThen (tuply andX)
        )


takeT3 : List a -> Maybe (T3 a a a)
takeT3 l =
    Maybe.map Tuple.first
        (andX T3 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT4 : List a -> Maybe (T4 a a a a)
takeT4 l =
    Maybe.map Tuple.first
        (andX T4 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT5 : List a -> Maybe (T5 a a a a a)
takeT5 l =
    Maybe.map Tuple.first
        (andX T5 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT6 : List a -> Maybe (T6 a a a a a a)
takeT6 l =
    Maybe.map Tuple.first
        (andX T6 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT7 : List a -> Maybe (T7 a a a a a a a)
takeT7 l =
    Maybe.map Tuple.first
        (andX T7 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT8 : List a -> Maybe (T8 a a a a a a a a)
takeT8 l =
    Maybe.map Tuple.first
        (andX T8 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT9 : List a -> Maybe (T9 a a a a a a a a a)
takeT9 l =
    Maybe.map Tuple.first
        (andX T9 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT10 : List a -> Maybe (T10 a a a a a a a a a a)
takeT10 l =
    Maybe.map Tuple.first
        (andX T10 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT11 : List a -> Maybe (T11 a a a a a a a a a a a)
takeT11 l =
    Maybe.map Tuple.first
        (andX T11 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )


takeT12 : List a -> Maybe (T12 a a a a a a a a a a a a)
takeT12 l =
    Maybe.map Tuple.first
        (andX T12 l
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
            |> Maybe.andThen (tuply andX)
        )



-- takeT2 : List a -> Maybe (T2 a)
