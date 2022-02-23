module Evergreen.V1.TranslationParser exposing (..)

import AssocList
import Elm.Syntax.Expression
import Elm.Syntax.Range
import List.Nonempty


type Content
    = TextContent String
    | Placeholder Elm.Syntax.Expression.Expression


type alias TranslationValue_ =
    { value : List.Nonempty.Nonempty Content
    , range : Elm.Syntax.Range.Range
    , isMarkdown : Maybe Elm.Syntax.Expression.Expression
    }


type alias TranslationDeclaration =
    { filePath : String
    , functionName : String
    , language : String
    , translations : AssocList.Dict (List.Nonempty.Nonempty String) (Result () TranslationValue_)
    }
