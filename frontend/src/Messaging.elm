module Messaging exposing (CSV, File, Model, Msg(..), ReadRow, Status(..), parseCsv)

import Csv.Decode as CDecode exposing (Error, FieldNames(..), blank, decodeCsv, field, into, pipeline, string)
import Http



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


type alias Model =
    { status : Status
    , file : Maybe File
    , fileList : List File
    }



--Update


type Msg
    = GotFileList (Result Http.Error (List String))
    | GetCSV String
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
            [ into ReadRow
                |> pipeline (field "BookTitle" string)
                |> pipeline (field "OptTag" (blank string))
                |> pipeline (CDecode.succeed Nothing)
            ]
        )
        str
