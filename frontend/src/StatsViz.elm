module StatsViz exposing (parseCSV, yearView)

import Dict exposing (Dict)
import Debug exposing (toString)
import Html exposing (div, Html, text, h1, p)
import String as S
import Messaging exposing (Msg, CSV, ReadRow)
import NoteParser exposing (ReadingNote(..), parse, stripString, noteEnum, formatReadingNote)


type alias Date =
  { year: Int
  , month: Int
  , day: Int
  }

type alias ParsedReadRow =
  { book_title: String
  , parsed_note : List ReadingNote
  , date: Date
  }

type alias ParsedCSV =
  List ParsedReadRow

type alias MonthTotals = Dict (Int, String) Int

type alias CSVStats = Dict String Int

listToDate : List String -> Date
listToDate ls =
    let
      intLs = List.map (Maybe.withDefault 0 << S.toInt) ls
    in
    case intLs of
      day :: month :: year :: [] -> Date (2000+year) month day
      _ -> Date 0 0 0

parseDate : Maybe String -> Date
parseDate str =
  case str of
    Just date ->  listToDate <| List.map stripString <| String.split "/" <| String.slice 1 -1 (stripString date)
    Nothing -> Date 0 0 0

parseRow : ReadRow -> ParsedReadRow
parseRow row = ParsedReadRow row.book_title (parse row.reading_note) (parseDate row.date)

formatDate : ParsedReadRow -> String
formatDate row = S.fromInt row.date.year ++ "/" ++ S.fromInt row.date.month ++ "/" ++ S.fromInt row.date.day

matchType : ReadingNote -> ParsedReadRow -> Int
matchType note_type row =
  case row.parsed_note of
    [] -> 0
    a :: rest -> (if a == note_type then 1 else 0)
      + matchType note_type (ParsedReadRow row.book_title rest row.date)

totalType : ParsedCSV -> ReadingNote -> Int
totalType csv note_type = List.foldl (+) 0 (List.map (matchType note_type) csv)

parseCSV : CSV -> ParsedCSV
parseCSV = List.map parseRow

count : List a -> Int
count ls = List.sum <| List.map (\_ -> 1) ls

singleMonth : List Date -> Int -> Int
singleMonth dates d = count <| (List.filter ((==) d) (List.map (\n -> n.month) dates))

formatMonth : Int -> String
formatMonth m
  = case m of
    1 -> "Jan"
    2 -> "Feb"
    3 -> "Mar"
    4 -> "Apr"
    5 -> "May"
    6 -> "Jun"
    7 -> "Jul"
    8 -> "Aug"
    9 -> "Sep"
    10 -> "Oct"
    11 -> "Nov"
    12 -> "Dec"
    _ -> "Unknown Month"

totalMonths : ParsedCSV -> MonthTotals
totalMonths csv = Dict.fromList (List.map2 Tuple.pair (List.map2 Tuple.pair (List.range 1 12) (List.map formatMonth (List.range 1 12))) (List.map (singleMonth (List.map (\n -> n.date) csv)) (List.range 1 12)))

yearView : ParsedCSV -> Html Msg
yearView csv = let
                 dates = List.map formatDate csv
                 counts = totalMonths csv
                 emptyDates = List.sum (Dict.values counts)
                 total = count csv
                 gStats = Dict.fromList (List.map2 Tuple.pair (List.map formatReadingNote noteEnum) (List.map (totalType csv) noteEnum))
               in
                 div [] [ h1 [] [text "Stats"]
                        , p [] [text <| S.join "\n" (List.map formatDate csv)]
                        , p [] [ text <| "Month Totals: " ++ toString ( List.map2 Tuple.pair (Dict.keys counts) (Dict.values counts))]
                        , p [] [ text <| "Month Totals: " ++ toString ( List.map2 Tuple.pair (Dict.keys gStats) (Dict.values gStats))]
                        ]
