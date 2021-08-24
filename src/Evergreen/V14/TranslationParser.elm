module Evergreen.V14.TranslationParser exposing (..)

import AssocList
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Signature
import List.Nonempty


type Content
    = TextContent String
    | Placeholder Elm.Syntax.Expression.Expression


type alias TranslationDeclaration =
    { filePath : String
    , documentation : Maybe (Elm.Syntax.Node.Node String)
    , signature : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Signature.Signature)
    , functionName : String
    , parameters : List Elm.Syntax.Pattern.Pattern
    , language : String
    , translations :
        AssocList.Dict
            (List.Nonempty.Nonempty String)
            (Result
                ()
                { value : List.Nonempty.Nonempty Content
                , range : Elm.Syntax.Range.Range
                }
            )
    }
