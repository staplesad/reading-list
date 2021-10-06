module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import String as S exposing (cons, reverse, uncons)

import Homepage exposing (svg_main)
import Messaging exposing (File, Model, Msg(..), ReadRow, Status(..), parseCsv)
import NoteParser exposing (..)

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



-- Format Csv



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
