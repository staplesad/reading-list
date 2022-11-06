module StatsViz exposing (yearView, getAllStats)

import Debug exposing (toString)
import Dict
import Html exposing (div, Html, text, h1, p)
import String as S
import Messaging exposing (Date, ParsedCSV, CSV, ReadRow, ParsedReadRow, MonthTotals, CSVStats, AllStats, Msg)
import NoteParser exposing (ReadingNote(..), parse, stripString, noteEnum, formatReadingNote)



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

getAllStats : CSV -> AllStats
getAllStats csv =
  let
   pcsv = parseCSV csv
   counts = totalMonths pcsv
   emptyDates = List.sum (Dict.values counts)
   total = count pcsv
   gStats = Dict.fromList (List.map2 Tuple.pair (List.map formatReadingNote noteEnum) (List.map (totalType pcsv) noteEnum))
  in
   case emptyDates of
     0 -> AllStats total Nothing gStats
     _ -> AllStats total (Just counts) gStats

yearView : String -> AllStats -> Html Msg
yearView year stats
  = case stats.monthly of
      Just monthly ->  div [] [ h1 [] [text <| "Stats: " ++ year]
             , p [] [ text <| "Total read: " ++ toString (stats.total)]
             , p [] [ text <| "Month Totals: " ++ toString ( List.map2 Tuple.pair (Dict.keys monthly) (Dict.values monthly))]
             , p [] [ text <| "Type Totals: " ++ toString ( List.map2 Tuple.pair (Dict.keys stats.types) (Dict.values stats.types))]
             ]
      Nothing -> div [] [ h1 [] [text <| "Stats: " ++ year]
             , p [] [ text <| "Total read: " ++ toString (stats.total)]
             , p [] [ text <| "Type Totals: " ++ toString ( List.map2 Tuple.pair (Dict.keys stats.types) (Dict.values stats.types))]
             ]

