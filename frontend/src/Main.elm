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
import Task
import Browser.Navigation as Nav
import Url
import Url.Builder
import Url.Parser as P exposing((</>))

import Homepage exposing (svg_main)
import Messaging exposing (File, Model, Msg(..), ReadRow, Status(..), BookId, parseCsv)
import NoteParser exposing (..)
import StatsViz exposing (yearView, getAllStats)
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
    (emptyModel url key
    , Cmd.batch [getFileList, getIdMap] )


emptyModel : Url.Url -> Nav.Key -> Model
emptyModel url key = Model Loading True Nothing [] Dct.empty url key Dct.empty Dct.empty

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

        GotIdMap maybeIdMap ->
          case maybeIdMap of
            Ok idMap -> ( {model | status = Index
                                 , idMap = idMap
                          }
                        , Cmd.none
                        )
            Err err ->
                    ( { model | status = Failure <| toString err
                              , idMap = Dct.empty
                      }
                    , Cmd.none )
        GetStats ->
          let
              filename = case model.file of
                Just f -> f.name
                Nothing -> ""
              stats = case filename of
                "" -> Nothing
                f -> case Dct.get f model.stats of
                  Just cached -> Just cached
                  Nothing -> case model.status of
                    Success csv -> Just (getAllStats csv)
                    _ -> Nothing
          in
              case stats of
              Just fileStats -> ( {model | status = ShowStats fileStats
                                         , stats = (Dct.update filename (\_ -> Just fileStats) model.stats)
                                         }
                                , Nav.pushUrl model.key (buildSUrl filename)
                                )
              Nothing -> ( {model | status = Failure "can't find filename or cached csv"}
                         , Cmd.none)

        GetBook title ->
          ( { model | status = Loading}
          , getBookData title -- map this to id
          )

        GotBook result ->
          let
              _ = log "library api" result
          in
            case result of
              Ok bookdata ->
                let
                    title = bookdata
                    id = Maybe.withDefault ("no id") <| Maybe.withDefault (Just "no entry") (Dct.get title model.idMap)
                in
                  ( { model | status = BookDisplay (BookId title id)}
                  , Cmd.none)
              Err err ->
                ( { model | status = Failure <| ("GotBook failure\n" ++ toString err)
                  }
                , Cmd.none
                )

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
              parsedUrl = parseUrl url
              _ = log "url changed" parsedUrl
              (mod, cmd)  = case parseUrl url of
                Just (Rows year) ->
                  ( { model | file = Just (File (toString year))
                            , status =
                               case Dct.get (toString year) model.csvs of
                                  Nothing -> Failure "list not cached"
                                  Just csv -> Success csv}
                  , Cmd.none)
                Just (Stats year) ->
                  ( {model | file = Just (File (toString year))
                          , status = case Dct.get (toString year) model.stats of
                              Nothing -> Failure "stats not cached"
                              Just stats -> ShowStats stats}
                  , Cmd.none)
                Just (Book title) ->
                  ({model | status = case Dct.get title model.idMap of
                            Just (Just id) -> BookDisplay <| BookId title id
                            _ -> Failure "can't find id"
                  }
                  , Cmd.none)

                Nothing ->
                   ( { model | status = Index
                                     , shouldAnimate = False
                                     , file = Nothing }
                   , Cmd.none)
          in
            ( { mod | url = url }
            , cmd
            )

type Page
  = Rows Int
  | Stats Int
  | Book String

flip : (a -> b -> c) -> (b -> a -> c)
flip f b a = f a b

parseLUrl : Url.Url -> Maybe Page
parseLUrl url = Maybe.map Rows (P.parse ((P.s "list") </> P.int) url)

parseSUrl : Url.Url -> Maybe Page
parseSUrl url = Maybe.map Stats (P.parse ((P.s "stats") </> P.int) url)

parseBUrl : Url.Url -> Maybe Page
parseBUrl url = Maybe.map Book (P.parse (P.s "book" </> P.string) url)

reduceMaybes : Maybe a -> Maybe a -> Maybe a
reduceMaybes a b = case (a, b) of
  (Just x, Nothing) -> Just x
  (Nothing, Just y) -> Just y
  _ -> Nothing

parseUrl : Url.Url -> Maybe Page
parseUrl url = List.foldl reduceMaybes Nothing (List.map (flip (<|) url) [parseLUrl, parseSUrl, parseBUrl])

buildUrl : String -> String
buildUrl filename = Url.Builder.absolute ["list", filename] []

buildSUrl : String -> String
buildSUrl filename = Url.Builder.absolute ["stats", filename] []

buildBUrl : String -> String
buildBUrl title = Url.Builder.absolute ["book", title] []

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
  let
      _ = log "debug view" <| toString model.status
  in
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

        ShowStats stats ->
          let
              filename = case model.file of
                Nothing -> ""
                Just f -> f.name
          in
          { title = "Stats " ++ filename
          , body = [viewStats filename stats]
          }

        BookDisplay id ->
          {title = "Book " ++ id.title
          , body = [viewBook model]
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
            div [] [ button [onClick GetStats] [text "Display Stats"]
                   , displayRList csv model.file ]

        _ ->
            div [] []

viewStats : String -> Messaging.AllStats -> Html Msg
viewStats filename stats = div [] [ yearView filename stats
                                  , div [ style "text-align" "center" ] [ button [ onClick Return ] [ text "Go back" ] ]
                                  ]

viewBook : Model -> Html Msg
viewBook model =
  case model.status of
    Failure err ->
      div [] [text ("Couldn't Load:\t"++ err)]

    BookDisplay id -> div [] [text (id.title ++ "\n" ++ id.id)]

    _ -> div [] []

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

toLowerCase : String -> String
toLowerCase str =
  case S.uncons str of
    Just (c, rest) ->
      S.cons (Char.toLower c) (toLowerCase rest)
    Nothing ->
      ""

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
        [ div [ class classname, style "width" "33%" ] [
          if classname /= "header"
          then a [ class "fakelink"
                 , href (buildBUrl (toLowerCase text1) )
                 , onClick <| GetBook (toLowerCase text1)][text text1]
          else (text text1)
          ]
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

run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())

-- will change title to id
-- need to make call to Open Lib url
getBookData : String -> Cmd Msg
getBookData title = run (GotBook (Ok title))

getFileList : Cmd Msg
getFileList =
    Http.get
        { url = "/files.json"
        , expect = Http.expectJson GotFileList fileDecoder
        }

getIdMap : Cmd Msg
getIdMap =
    Http.get
        { url = "/olidMap.json"
        , expect = Http.expectJson GotIdMap idDecoder
        }


fileDecoder : D.Decoder (List String)
fileDecoder =
    D.list D.string


idDecoder : D.Decoder (Dict String (Maybe String))
idDecoder = D.dict (D.nullable D.string)
