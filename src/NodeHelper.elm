module NodeHelper exposing (noRangeExpression, node)

{-| Helper functions copied from stil4m/elm-syntax
-}

import Elm.Syntax.Expression exposing (..)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (..)
import Elm.Syntax.Range exposing (emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (..)


node : a -> Node a
node =
    Node { start = { row = 0, column = 0 }, end = { row = 0, column = 0 } }


noRangeExpression : Node Expression -> Node Expression
noRangeExpression (Node _ inner) =
    Node emptyRange <| noRangeInnerExpression inner


noRangePattern : Node Pattern -> Node Pattern
noRangePattern (Node _ p) =
    Node emptyRange <|
        case p of
            RecordPattern ls ->
                RecordPattern (List.map unRange ls)

            VarPattern x ->
                VarPattern x

            NamedPattern x y ->
                NamedPattern x (List.map noRangePattern y)

            ParenthesizedPattern x ->
                ParenthesizedPattern (noRangePattern x)

            AsPattern x y ->
                AsPattern (noRangePattern x) (unRange y)

            UnConsPattern x y ->
                UnConsPattern (noRangePattern x) (noRangePattern y)

            CharPattern c ->
                CharPattern c

            StringPattern s ->
                StringPattern s

            HexPattern h ->
                HexPattern h

            FloatPattern f ->
                FloatPattern f

            IntPattern i ->
                IntPattern i

            AllPattern ->
                AllPattern

            UnitPattern ->
                UnitPattern

            ListPattern x ->
                ListPattern (List.map noRangePattern x)

            TuplePattern x ->
                TuplePattern (List.map noRangePattern x)


unRange : Node a -> Node a
unRange n =
    unRanged identity n


unRanged : (a -> a) -> Node a -> Node a
unRanged f (Node _ a) =
    Node emptyRange <| f a


noRangeLetDeclaration : Node LetDeclaration -> Node LetDeclaration
noRangeLetDeclaration (Node _ decl) =
    Node emptyRange <|
        case decl of
            LetFunction function ->
                LetFunction (noRangeFunction function)

            LetDestructuring pattern expression ->
                LetDestructuring (noRangePattern pattern) (noRangeExpression expression)


noRangeRecordField : RecordField -> RecordField
noRangeRecordField ( a, b ) =
    ( unRange a, noRangeTypeReference b )


noRangeRecordDefinition : RecordDefinition -> RecordDefinition
noRangeRecordDefinition =
    List.map (unRanged noRangeRecordField)


noRangeTypeReference : Node TypeAnnotation -> Node TypeAnnotation
noRangeTypeReference (Node _ typeAnnotation) =
    Node emptyRange <|
        case typeAnnotation of
            GenericType x ->
                GenericType x

            Typed (Node _ ( a, b )) c ->
                Typed (Node emptyRange ( a, b )) (List.map noRangeTypeReference c)

            Unit ->
                Unit

            Tupled a ->
                Tupled (List.map noRangeTypeReference a)

            Record a ->
                Record (List.map (unRanged noRangeRecordField) a)

            GenericRecord a b ->
                GenericRecord (unRanged identity a) (unRanged noRangeRecordDefinition b)

            FunctionTypeAnnotation a b ->
                FunctionTypeAnnotation
                    (noRangeTypeReference a)
                    (noRangeTypeReference b)


noRangeFunction : Function -> Function
noRangeFunction f =
    { f
        | declaration = unRanged noRangeFunctionImplementation f.declaration
        , signature = Maybe.map (unRanged noRangeSignature) f.signature
    }


noRangeSignature : Signature -> Signature
noRangeSignature signature =
    { signature | typeAnnotation = noRangeTypeReference signature.typeAnnotation, name = unRange signature.name }


noRangeFunctionImplementation : FunctionImplementation -> FunctionImplementation
noRangeFunctionImplementation d =
    { d
        | expression = noRangeExpression d.expression
        , arguments = List.map noRangePattern d.arguments
        , name = unRange d.name
    }


noRangeRecordSetter : RecordSetter -> RecordSetter
noRangeRecordSetter ( a, b ) =
    ( unRange a, unRanged noRangeInnerExpression b )


noRangeInnerExpression : Expression -> Expression
noRangeInnerExpression inner =
    case inner of
        Application xs ->
            Application <| List.map noRangeExpression xs

        OperatorApplication op dir left right ->
            OperatorApplication op dir (noRangeExpression left) (noRangeExpression right)

        ListExpr xs ->
            ListExpr <| List.map noRangeExpression xs

        IfBlock a b c ->
            IfBlock
                (noRangeExpression a)
                (noRangeExpression b)
                (noRangeExpression c)

        RecordExpr fields ->
            RecordExpr <| List.map (unRanged noRangeRecordSetter) fields

        LambdaExpression lambda ->
            LambdaExpression
                { lambda
                    | expression = noRangeExpression lambda.expression
                    , args = List.map noRangePattern lambda.args
                }

        RecordUpdateExpression name updates ->
            RecordUpdateExpression (unRanged identity name) (List.map (unRanged noRangeRecordSetter) updates)

        CaseExpression { cases, expression } ->
            CaseExpression
                { cases =
                    cases
                        |> List.map (Tuple.mapFirst noRangePattern)
                        |> List.map (Tuple.mapSecond noRangeExpression)
                , expression = noRangeExpression expression
                }

        LetExpression { declarations, expression } ->
            LetExpression
                { declarations = List.map noRangeLetDeclaration declarations
                , expression = noRangeExpression expression
                }

        TupledExpression x ->
            TupledExpression <| List.map noRangeExpression x

        ParenthesizedExpression x ->
            ParenthesizedExpression <| noRangeExpression x

        RecordAccess e n ->
            RecordAccess (noRangeExpression e) (unRange n)

        Negation expr ->
            Negation (noRangeExpression expr)

        _ ->
            inner
