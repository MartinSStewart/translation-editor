module Evergreen.V2.Cache exposing (..)

import AssocList
import Dict
import Elm.Syntax.Range
import Evergreen.V2.TranslationParser
import List.Nonempty


type alias CachedTranslation =
    { functionName : String
    , translations :
        AssocList.Dict
            (List.Nonempty.Nonempty String)
            (Result
                ()
                { value : List.Nonempty.Nonempty Evergreen.V2.TranslationParser.Content
                , range : Elm.Syntax.Range.Range
                }
            )
    }


type alias Cache =
    Dict.Dict Int (List CachedTranslation)
