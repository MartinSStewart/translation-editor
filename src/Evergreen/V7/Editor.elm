module Evergreen.V7.Editor exposing (..)

import List.Nonempty


type alias TranslationId =
    { filePath : String
    , functionName : String
    , path : List.Nonempty.Nonempty String
    }
