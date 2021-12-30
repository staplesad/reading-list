module NoteParser exposing (parse, stripString, formatReadingNote, ReadingNote(..), noteEnum)

import Debug exposing (toString)
import Parser as P exposing ((|.), (|=), Parser, oneOf, succeed, symbol, token, variable)
import Set
import String as S

-- Parser

type ReadingNote
    = Reread
    | ShortStoryCollection
    | ShortStory
    | NonFiction
    | GraphicNovel
    | Play
    | Poetry
    | Borrowed String
    | NoteError String

noteEnum : List ReadingNote
noteEnum = [Reread, ShortStoryCollection, ShortStory, NonFiction, GraphicNovel, Play, Poetry]

cons : a -> List a -> List a
cons x xs =
    x :: xs


many : Parser a -> Parser (List a)
many p =
    oneOf
        [ P.map (\_ -> []) P.end
        , P.andThen (\a -> P.map (cons a) (many p)) p
        ]


parseReadingNote : Parser ReadingNote
parseReadingNote =
    oneOf
        [ reread
        , shortstories
        , shortstory
        , nonfiction
        , graphicnovel
        , play
        , poetry
        , borrowed
        ]


reread : Parser ReadingNote
reread =
    succeed Reread
        |. token "*"


shortstories =
    succeed ShortStoryCollection
        |. token "^^"


shortstory =
    succeed ShortStory
        |. token "^"


nonfiction =
    succeed NonFiction
        |. token "o"


graphicnovel =
    succeed GraphicNovel
        |. token "l"


play =
    succeed Play
        |. token "$"


poetry =
    succeed Poetry
        |. token "&"


borrowed =
    succeed Borrowed
        |. oneOf
            [ symbol "("
            , symbol "["
            ]
        |= variable
            { start = Char.isAlpha
            , inner = Char.isAlpha
            , reserved = Set.empty
            }
        |. oneOf
            [ symbol ")"
            , symbol "]"
            ]


getName : String -> String
getName name =
    case String.length name of
        1 ->
            case name of
                "J" ->
                    "Jack"

                "R" ->
                    "Jenny"

                "Z" ->
                    "Jamie"

                "C" ->
                    "Ciara"

                "H" ->
                    "Harriet"

                "P" ->
                    "Philip"

                _ ->
                    "<unk>"

        _ ->
            name


formatReadingNote : ReadingNote -> String
formatReadingNote maybeNote =
    case maybeNote of
        note ->
            case note of
                Reread ->
                    "reread"

                ShortStory ->
                    "short story"

                ShortStoryCollection ->
                    "short piece collection"

                NonFiction ->
                    "non-fiction"

                GraphicNovel ->
                    "graphic novel"

                Play ->
                    "play"

                Poetry ->
                    "poetry"

                Borrowed c ->
                    "given to me by " ++ getName c

                NoteError str ->
                    "Parse error:" ++ str

stripLeft : String -> String
stripLeft str =
    case S.uncons str of
        Just ( c, rest ) ->
            if c /= ' ' then
                S.cons c rest

            else
                stripLeft rest

        Nothing ->
            ""


stripRight : String -> String
stripRight =
    S.reverse << stripLeft << S.reverse


stripString : String -> String
stripString =
    stripRight << stripLeft

parse : Maybe String -> List ReadingNote
parse str =
    case str of
        Just note ->
            case P.run (many parseReadingNote) (stripString note) of
                Ok notes ->
                    notes

                Err a ->
                    [ NoteError (toString a), NoteError note ]

        Nothing ->
            []
