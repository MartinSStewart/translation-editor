module Evergreen.V1.Cache exposing (..)

import AssocList
import Dict
import Evergreen.V1.TranslationParser
import List.Nonempty


type alias CachedTranslation =
    { functionName : String
    , translations : AssocList.Dict (List.Nonempty.Nonempty String) (Result () Evergreen.V1.TranslationParser.TranslationValue_)
    }


type alias Cache =
    Dict.Dict Int (List CachedTranslation)
