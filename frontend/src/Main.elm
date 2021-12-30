module Main exposing (..)

import Browser
import Debug exposing (toString, log)
import Dict as Dct exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import String as S
import Browser.Navigation as Nav
import Url
import Url.Builder
import Url.Parser as P exposing((</>))

import Homepage exposing (svg_main)
import Messaging exposing (File, Model, Msg(..), ReadRow, Status(..), parseCsv)
import NoteParser exposing (..)
import StatsViz exposing (parseCSV, yearView)
-- Main


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model Loading True Nothing [] url key Dct.empty, getFileList )



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
          let
              csv = Dct.get filename model.csvs
          in
              case csv of
                Just cached -> ({ model | file = Just(File filename)
                                        , status = Success cached}
                               , Nav.pushUrl model.key (buildUrl filename)
                               )
                Nothing -> ( { model | status = Loading
                                     , file = Just (File filename) }
                           , getCSVReq filename
                           )

        GotCSV result ->
            case result of
                Ok csv ->
                  let
                      filename = case model.file of
                        Just f -> f.name
                        Nothing -> "..."
                      res = case parseCsv csv of
                          Ok parsedCsv ->
                              Success parsedCsv

                          Err err ->
                              Failure <| toString err
                  in
                    case res of
                      Success pres -> ( { model
                                    | csvs = Dct.update filename (\_ -> Just pres) model.csvs
                                    , status = res
                                    }
                                  , Nav.pushUrl model.key (buildUrl filename)
                                  )
                      Failure _ -> ( { model | status = res}
                                , Nav.pushUrl model.key (buildUrl filename)
                                )
                      _ -> ( {model | status = res}, Cmd.none)

                Err err ->
                    ( { model | status = Failure <| toString err }, Cmd.none )

        Return ->
            ( { model | status = Index, shouldAnimate = False, file = Nothing }
            , (Nav.pushUrl model.key (Url.Builder.absolute [] []))
            )

        LinkClicked urlRequest ->
          (model, Cmd.none)

        UrlChanged url ->
          let
              (mod, cmd)  = case parseUrl url of
                        Just year -> ( { model | file = Just (File (toString year))
                                               , status =
                                                 case Dct.get (toString year) model.csvs of
                                                    Nothing -> Failure "not cached"
                                                    Just csv -> Success csv}
                                     , Cmd.none)
                        Nothing -> ( { model | status = Index
                                             , shouldAnimate = False
                                             , file = Nothing }
                                   , Cmd.none
                                   )
          in
            ( { mod | url = url }
            , cmd
            )

parseUrl : Url.Url -> Maybe Int
parseUrl = P.parse (P.s "list" </> P.int)

buildUrl : String -> String
buildUrl filename = Url.Builder.absolute ["list", filename] []

-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


fileToString : File -> String
fileToString file =
    file.name


view : Model -> Browser.Document Msg
view model =
    case model.status of
        Index ->
          { title = "Index"
            , body =
              [div []
                [ h1 [] [ text "Books I've Read" ]
                , svg_main (List.map fileToString model.fileList) model.shouldAnimate
                ]
              ]
          }

        _ ->
          { title = (case model.file of
                        Just f -> f.name
                        Nothing -> "...")

            , body =
              [div [] [ viewCsv model ]]
          }


viewCsv : Model -> Html Msg
viewCsv model =
    case model.status of
        Failure err ->
            div [] [ text ("Couldn't Load:\t" ++ err) ]

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
        -- [ yearView (parseCSV csv)
        [ h1 [ style "text-align" "center" ] [ text <| "Read in: " ++ formatFileName file ]
        , div [ style "display" "flex", style "flex-direction" "column", style "width" "75%", style "margin" "auto" ] (classDiv "header" "Book Title" "Book Info" "Date Finished" :: List.indexedMap mapToRowDiv csv)
        , div [ style "text-align" "center" ] [ button [ onClick Return ] [ text "Go back" ] ]
        ]



-- Http

csvUrlBuilder : String -> String
csvUrlBuilder filename = Url.Builder.absolute [filename] []

getCSVReq : String -> Cmd Msg
getCSVReq filename =
    Http.get
        { url = csvUrlBuilder filename
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
