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

type alias BookId =
  { title : String
  , id : String
  }
type Status
    = Failure String
    | Index
    | Loading
    | Success CSV
    | ShowStats AllStats
    | BookDisplay BookId



type alias Model =
    { status : Status
    , shouldAnimate: Bool
    , file : Maybe File
    , fileList : List File
    , idMap : Dict.Dict String (Maybe String)
    , url: Url.Url
    , key: Nav.Key
    , csvs: Dict.Dict String CSV
    , stats: Dict.Dict String AllStats
    }



--Update


type Msg
    = GotFileList (Result Http.Error (List String))
    | GotIdMap (Result Http.Error (Dict.Dict String (Maybe String)))
    | GetBook String
    | GotBook (Result Http.Error String)
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
