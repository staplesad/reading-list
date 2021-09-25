module Main exposing (..)

import Browser
import Csv.Decode as CDecode exposing (decodeCsv, FieldNames(..), string, blank, pipeline, field, into, Error)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Parser as P exposing (oneOf, Parser, token, (|.), (|=), succeed, symbol, variable)
import Debug exposing (toString)
import Set

-- Main
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- Model

type alias File =
  { name: String }

type alias ReadRow = {
  book_title: String,
  reading_note: Maybe String,
  date: Maybe String
  }

type alias CSV = List ReadRow

type Status
  = Failure String
  | Index
  | Loading
  | Success CSV

type alias Model = {
  status: Status,
  file: Maybe File,
  fileList: List File
  }

init: () -> (Model, Cmd Msg)
init _ = (Model Loading Nothing [], getFileList)


-- Update

type Msg
  = GotFileList (Result Http.Error (List String))
  | GetCSV File
  | GotCSV (Result Http.Error String)
  | Return

parseCsv : String -> Result Error CSV
parseCsv str = 
  decodeCsv FieldNamesFromFirstRow
    (CDecode.oneOf
      (into ReadRow
       |> pipeline (field "BookTitle" string)
       |> pipeline (field "OptTag" (blank string))
       |> pipeline (field "Date" (blank string))
      )
      [ (into ReadRow
                   |> pipeline (field "BookTitle" string)
                   |> pipeline (field "OptTag" (blank string))
                   |> pipeline (CDecode.succeed Nothing)
                  )
      ]
    )
    str

stringToFile : String -> File
stringToFile str = File str

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFileList maybeFileList ->
          case maybeFileList of
            Ok fileList -> ({model | status = Index, fileList = List.map stringToFile fileList}, Cmd.none)
            Err err -> ({model | status = Failure <| toString err, fileList = []}, Cmd.none)
        GetCSV file -> ({model| status = Loading, file = Just file}, getCSVReq file.name)
        GotCSV result ->
          case result of
            Ok csv ->
              ({model| status = case parseCsv csv of
                                  Ok parsedCsv -> Success parsedCsv
                                  Err err -> Failure <| toString err}, Cmd.none)
            Err err ->
              ({model | status = Failure <| toString err}, Cmd.none)
        Return ->
          (Model Index Nothing model.fileList, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
      Sub.none

-- View

viewFile : File -> Html Msg
viewFile file = div [] [ button [onClick (GetCSV file)] [text file.name]]

view : Model -> Html Msg
view model =
  case model.status of
    Index -> div [] (List.append [ h1 [] [text "Get CSV File"]]
                               (List.map viewFile model.fileList))
    _ -> div [] [ viewCsv model]


viewCsv: Model -> Html Msg
viewCsv model =
    case model.status of
        Failure err -> div [] [text ("Couldn't Load" ++ err) ]
        Loading -> div [] [text "Loading..."]
        Success csv -> div [] [ displayRList csv model.file]
        _ -> div [] []


-- Parser
type ReadingNote = Reread
                 | ShortStoryCollection
                 | ShortStory
                 | NonFiction
                 | GraphicNovel
                 | Play
                 | Poetry
                 | Borrowed String
                 | NoteError String

cons : a -> List a -> List a
cons x xs = x :: xs

many : Parser a -> Parser (List a)
many p = oneOf
  [ P.map (\_ -> []) P.end
  , P.andThen (\a -> P.map (cons a) (many p)) p
  ]

parseReadingNote: Parser ReadingNote
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
    |. token "•"

graphicnovel =
  succeed GraphicNovel
    |. token "π"

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
getName name = case String.length name of
  1 -> case name of
    "J" -> "Jack"
    "R" -> "Jenny"
    "Z" -> "Jamie"
    "C" -> "Ciara"
    "H" -> "Harriet"
    "P" -> "Philip"
    _ -> "<unk>"
  _ -> name

formatReadingNote : ReadingNote -> String
formatReadingNote maybeNote =
  case maybeNote of
    note -> case note of
      Reread -> "reread"
      ShortStory -> "short story"
      ShortStoryCollection -> "short piece collection"
      NonFiction -> "non-fiction"
      GraphicNovel -> "graphic novel"
      Play -> "play"
      Poetry -> "poetry"
      Borrowed c -> "given to me by " ++ (getName c)
      NoteError str -> "Parse error:" ++ str

parse : Maybe String -> List ReadingNote
parse str = case str of
  Just note ->
    case P.run (many parseReadingNote) note of
      Ok notes -> notes
      Err a -> [NoteError (toString a)]
  Nothing -> []

-- Format Csv

mapToDiv: Int -> (String -> a -> Html Msg) -> a -> Html Msg
mapToDiv idx spefDiv item = if (modBy 2 idx == 1)
                            then spefDiv "#afeeee" item
                            else spefDiv "#ffffff" item

mapToRowDiv : Int -> ReadRow -> Html Msg
mapToRowDiv idx csv = mapToDiv idx rowDiv csv

formatDate : Maybe String -> String
formatDate str =
  case str of
    Just date -> String.slice 1 -1 date
    Nothing -> ""

rowDiv : String -> ReadRow -> Html Msg
rowDiv color csv = div [style "display" "flex", style "flex-direction" "row", style "justify-content" "space-around"] 
  [ div [style "background-color" color, style "width" "33%"] [text csv.book_title]
  , div [style "background-color" color, style "width" "33%"] [text (String.join ", " (List.map formatReadingNote ( parse csv.reading_note)))]
  , div [style "background-color" color, style "width" "33%"] [text (formatDate csv.date)]
  ]

formatFileName : Maybe File -> String
formatFileName file =
  case file of
    Nothing -> ""
    Just fp -> fp.name

titleList : ReadRow
titleList = ReadRow "Book Title" (Just "Book Info") (Just "Date Finished")

displayRList : List ReadRow -> Maybe File -> Html Msg
displayRList csv file = div [style "margin" "auto"] 
  [  h1 [style "text-align" "center"] [text <| "Read in: " ++ formatFileName file]
  ,  div [style "display" "flex", style "flex-direction" "column", style "width" "75%", style "margin" "auto"] (List.indexedMap mapToRowDiv (titleList :: csv))
  , div [style "text-align" "center"] [button [onClick Return][text "Go back"]]
  ]

-- Http

getCSVReq : String -> Cmd Msg
getCSVReq filename = Http.get { url = filename
                               , expect = Http.expectString GotCSV}

getFileList : Cmd Msg
getFileList = Http.get {url = "/files.json"
                        , expect = Http.expectJson GotFileList fileDecoder}

fileDecoder : D.Decoder (List String)
fileDecoder = D.list D.string

