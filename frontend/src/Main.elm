module Main exposing (..)

import Browser
import Debug exposing (toString)
import Homepage exposing (svg_main)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Messaging exposing (File, Model, Msg(..), ReadRow, Status(..), parseCsv)
import Parser as P exposing ((|.), (|=), Parser, oneOf, succeed, symbol, token, variable)
import Set
import String as S exposing (cons, reverse, uncons)



-- Main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading True Nothing [], getFileList )



-- Update


stringToFile : String -> File
stringToFile str =
    File str


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFileList maybeFileList ->
            case maybeFileList of
                Ok fileList ->
                    ( { model | status = Index, fileList = List.map stringToFile fileList }, Cmd.none )

                Err err ->
                    ( { model | status = Failure <| toString err, fileList = [] }, Cmd.none )

        GetCSV filename ->
            ( { model | status = Loading, file = Just (File filename) }, getCSVReq filename )

        GotCSV result ->
            case result of
                Ok csv ->
                    ( { model
                        | status =
                            case parseCsv csv of
                                Ok parsedCsv ->
                                    Success parsedCsv

                                Err err ->
                                    Failure <| toString err
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | status = Failure <| toString err }, Cmd.none )

        Return ->
            ( Model Index False Nothing model.fileList, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


fileToString : File -> String
fileToString file =
    file.name


view : Model -> Html Msg
view model =
    case model.status of
        Index ->
            div []
                [ h1 [] [ text "Books I've Read" ]
                , svg_main (List.map fileToString model.fileList) model.shouldAnimate
                ]

        _ ->
            div [] [ viewCsv model ]


viewCsv : Model -> Html Msg
viewCsv model =
    case model.status of
        Failure err ->
            div [] [ text ("Couldn't Load" ++ err) ]

        Success csv ->
            div [] [ displayRList csv model.file ]

        _ ->
            div [] []



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



-- Format Csv


stripLeft : String -> String
stripLeft str =
    case uncons str of
        Just ( c, rest ) ->
            if c /= ' ' then
                S.cons c rest

            else
                stripLeft rest

        Nothing ->
            ""


stripRight : String -> String
stripRight =
    reverse << stripLeft << reverse


stripString : String -> String
stripString =
    stripRight << stripLeft


firstUpper : String -> String
firstUpper str =
    case S.uncons str of
        Just ( c, rest ) ->
            S.cons (Char.toUpper c) rest

        Nothing ->
            ""


toTitleCase : String -> String
toTitleCase str =
    S.join " " (List.map firstUpper <| S.words str)


mapToDiv : Int -> (String -> a -> Html Msg) -> a -> Html Msg
mapToDiv idx spefDiv item =
    if modBy 2 idx == 1 then
        spefDiv "oddrow" item

    else
        spefDiv "evenrow" item


mapToRowDiv : Int -> ReadRow -> Html Msg
mapToRowDiv idx csv =
    mapToDiv idx rowDiv csv


formatDate : Maybe String -> String
formatDate str =
    case str of
        Just date ->
            String.slice 1 -1 (stripString date)

        Nothing ->
            ""


rowDiv : String -> ReadRow -> Html Msg
rowDiv classname csv =
    classDiv classname (toTitleCase csv.book_title) (String.join ", " (List.map formatReadingNote (parse csv.reading_note))) (formatDate csv.date)


classDiv : String -> String -> String -> String -> Html Msg
classDiv classname text1 text2 text3 =
    div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "space-around" ]
        [ div [ class classname, style "width" "33%" ] [ text text1 ]
        , div [ class classname, style "width" "33%" ] [ text text2 ]
        , div [ class classname, style "width" "33%" ] [ text text3 ]
        ]


formatFileName : Maybe File -> String
formatFileName file =
    case file of
        Nothing ->
            ""

        Just fp ->
            fp.name


displayRList : List ReadRow -> Maybe File -> Html Msg
displayRList csv file =
    div [ style "margin" "auto" ]
        [ h1 [ style "text-align" "center" ] [ text <| "Read in: " ++ formatFileName file ]
        , div [ style "display" "flex", style "flex-direction" "column", style "width" "75%", style "margin" "auto" ] (classDiv "header" "Book Title" "Book Info" "Date Finished" :: List.indexedMap mapToRowDiv csv)
        , div [ style "text-align" "center" ] [ button [ onClick Return ] [ text "Go back" ] ]
        ]



-- Http


getCSVReq : String -> Cmd Msg
getCSVReq filename =
    Http.get
        { url = filename
        , expect = Http.expectString GotCSV
        }


getFileList : Cmd Msg
getFileList =
    Http.get
        { url = "/files.json"
        , expect = Http.expectJson GotFileList fileDecoder
        }


fileDecoder : D.Decoder (List String)
fileDecoder =
    D.list D.string
