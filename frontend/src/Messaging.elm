module Messaging exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Url

import Csv.Decode as CDecode exposing (Error, FieldNames(..), blank, decodeCsv, field, into, pipeline, string)
import Http
import NoteParser exposing (ReadingNote(..))

-- Model


type alias File =
    { name : String }


type alias ReadRow =
    { book_title : String
    , reading_note : Maybe String
    , date : Maybe String
    }


type alias CSV =
    List ReadRow


type Status
    = Failure String
    | Index
    | Loading
    | Success CSV
    | ShowStats AllStats


type alias Model =
    { status : Status
    , shouldAnimate: Bool
    , file : Maybe File
    , fileList : List File
    , url: Url.Url
    , key: Nav.Key
    , csvs: Dict.Dict String CSV
    , stats: Dict.Dict String AllStats
    }



--Update


type Msg
    = GotFileList (Result Http.Error (List String))
    | GetStats
    | GetCSV String
    | GotCSV (Result Http.Error String)
    | Return
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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

type alias AllStats =
  { total: Int
  , monthly: Maybe MonthTotals
  , types: CSVStats
  }

parseCsv : String -> Result Error CSV
parseCsv str =
    decodeCsv FieldNamesFromFirstRow
        (CDecode.oneOf
            (into ReadRow
                |> pipeline (field "BookTitle" string)
                |> pipeline (field "OptTag" (blank string))
                |> pipeline (field "Date" (blank string))
            )
            [ into ReadRow
                |> pipeline (field "BookTitle" string)
                |> pipeline (field "OptTag" (blank string))
                |> pipeline (CDecode.succeed Nothing)
            ]
        )
        str
