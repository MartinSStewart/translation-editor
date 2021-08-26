module TranslationParser exposing
    ( Content(..)
    , Translation(..)
    , TranslationDeclaration
    , TranslationValue
    , contentToString
    , expressionNameHelper
    , getLanguageLongName
    , getLanguageShortName
    , getPlaceholders
    , namesToPlaceholders
    , parse
    , placeholderToNames
    , writeContents
    )

import AssocList as Dict exposing (Dict)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import NodeHelper
import Parser exposing (DeadEnd)
import Set


type alias TranslationDeclaration =
    { filePath : String
    , documentation : Maybe (Node String)
    , signature : Maybe (Node Signature)
    , functionName : String
    , parameters : List Pattern
    , language : String
    , translations : Dict (Nonempty String) (Result () { value : Nonempty Content, range : Range })
    }


contentToString : Nonempty Content -> String
contentToString contents =
    let
        placeholderNames_ : Dict Expression String
        placeholderNames_ =
            getPlaceholders contents |> placeholderToNames
    in
    List.Nonempty.map
        (\content ->
            case content of
                TextContent text ->
                    text

                Placeholder expression ->
                    "{" ++ (Dict.get expression placeholderNames_ |> Maybe.withDefault "") ++ "}"
        )
        contents
        |> List.Nonempty.toList
        |> String.concat


getPlaceholders : Nonempty Content -> List Expression
getPlaceholders nonempty =
    List.Nonempty.toList nonempty
        |> List.filterMap
            (\content ->
                case content of
                    TextContent _ ->
                        Nothing

                    Placeholder expression ->
                        Just expression
            )


placeholderToNames : List Expression -> Dict Expression String
placeholderToNames expressions =
    List.indexedMap
        (\index expression ->
            expressionNameHelper expression |> Maybe.withDefault (String.fromInt index) |> Tuple.pair expression
        )
        expressions
        |> Dict.fromList


namesToPlaceholders : List Expression -> Dict String Expression
namesToPlaceholders expressions =
    List.indexedMap
        (\index expression ->
            ( expressionNameHelper expression |> Maybe.withDefault (String.fromInt index), expression )
        )
        expressions
        |> Dict.fromList


expressionNameHelper : Expression -> Maybe String
expressionNameHelper expression =
    case expression of
        UnitExpr ->
            Nothing

        Application nodes ->
            List.filterMap (Node.value >> expressionNameHelper) nodes |> List.reverse |> List.head

        OperatorApplication _ _ (Node _ left) (Node _ right) ->
            Maybe.Extra.or (expressionNameHelper left) (expressionNameHelper right)

        FunctionOrValue _ text ->
            Just text

        IfBlock condition ifTrue ifFalse ->
            List.filterMap (Node.value >> expressionNameHelper) [ condition, ifTrue, ifFalse ] |> List.head

        PrefixOperator _ ->
            Nothing

        Operator _ ->
            Nothing

        Integer _ ->
            Nothing

        Hex _ ->
            Nothing

        Floatable _ ->
            Nothing

        Negation (Node _ node) ->
            expressionNameHelper node

        Literal _ ->
            Nothing

        CharLiteral _ ->
            Nothing

        TupledExpression nodes ->
            List.filterMap (Node.value >> expressionNameHelper) nodes |> List.head

        ParenthesizedExpression (Node _ node) ->
            expressionNameHelper node

        LetExpression _ ->
            Nothing

        CaseExpression _ ->
            Nothing

        LambdaExpression _ ->
            Nothing

        RecordExpr _ ->
            Nothing

        ListExpr _ ->
            Nothing

        RecordAccess _ (Node _ field) ->
            Just field

        RecordAccessFunction field ->
            Just field

        RecordUpdateExpression _ _ ->
            Nothing

        GLSLExpression _ ->
            Nothing


patternName : Pattern -> Maybe String
patternName pattern =
    case pattern of
        AllPattern ->
            Nothing

        UnitPattern ->
            Nothing

        CharPattern char ->
            String.fromChar char |> Just

        StringPattern text ->
            Just text

        IntPattern int ->
            String.fromInt int |> Just

        HexPattern int ->
            String.fromInt int |> Just

        FloatPattern float ->
            String.fromFloat float |> Just

        TuplePattern _ ->
            Nothing

        RecordPattern _ ->
            Nothing

        UnConsPattern _ _ ->
            Nothing

        ListPattern _ ->
            Nothing

        VarPattern text ->
            Just text

        NamedPattern _ nodes ->
            case nodes of
                [ Node _ head ] ->
                    patternName head

                _ ->
                    Nothing

        AsPattern _ _ ->
            Nothing

        ParenthesizedPattern (Node _ node) ->
            patternName node


type Translation
    = Translation TranslationValue
    | ParseError { name : String, rowNumber : Int, expression : Expression }
    | Group { name : String, translations : List Translation }


type alias TranslationValue =
    { name : String, parameters : List Pattern, value : Node (Nonempty Content) }


errorMessage : Int -> String
errorMessage rowNumber =
    "Unsupported translation definition on line: " ++ String.fromInt rowNumber


type Content
    = TextContent String
    | Placeholder Expression



--codeGen : TranslationDeclaration -> String
--codeGen translationDeclaration =
--    { documentation = translationDeclaration.documentation
--    , signature = translationDeclaration.signature
--    , declaration =
--        NodeHelper.node
--            { name = NodeHelper.node translationDeclaration.functionName
--            , arguments = List.map NodeHelper.node translationDeclaration.parameters
--            , expression =
--                List.Nonempty.toList translationDeclaration.translations
--                    |> List.map writeTranslation
--                    |> RecordExpr
--                    |> NodeHelper.node
--            }
--    }
--        |> Elm.Pretty.prettyFun
--        |> Pretty.pretty 100
--
--writeTranslation : Translation -> Node RecordSetter
--writeTranslation translation =
--    case translation of
--        Translation { name, parameters, value } ->
--            NodeHelper.node
--                ( NodeHelper.node name
--                , if List.isEmpty parameters then
--                    writeContents value
--
--                  else
--                    LambdaExpression
--                        { args = List.map NodeHelper.node parameters
--                        , expression = writeContents value
--                        }
--                        |> NodeHelper.node
--                )
--
--        Group { name, translations } ->
--            List.map writeTranslation translations
--                |> RecordExpr
--                |> NodeHelper.node
--                |> Tuple.pair (NodeHelper.node name)
--                |> NodeHelper.node
--
--        ParseError { name, expression } ->
--            NodeHelper.node ( NodeHelper.node name, NodeHelper.node expression )


writeContents : Nonempty Content -> Expression
writeContents contents =
    List.Nonempty.concatMap
        (\content ->
            case content of
                TextContent text ->
                    let
                        lines =
                            String.lines text
                    in
                    List.indexedMap
                        (\index line ->
                            (if index == List.length lines - 1 then
                                line

                             else
                                line ++ "\n"
                            )
                                |> Literal
                                |> NodeHelper.node
                        )
                        lines
                        |> List.Nonempty.fromList
                        |> Maybe.withDefault (List.Nonempty.fromElement (NodeHelper.node (Literal text)))

                Placeholder value ->
                    List.Nonempty.fromElement (NodeHelper.node value)
        )
        contents
        |> List.Nonempty.reverse
        |> List.Nonempty.foldl1
            (\left right ->
                OperatorApplication "++" Right left right |> NodeHelper.node
            )
        |> Node.value


parseTranslationValue : Node Expression -> Nonempty Content
parseTranslationValue expression =
    case Node.value expression of
        OperatorApplication "++" _ left right ->
            List.Nonempty.append
                (parseTranslationValue left)
                (parseTranslationValue right)

        Literal text ->
            TextContent text |> List.Nonempty.fromElement

        _ ->
            NodeHelper.noRangeExpression expression
                |> Node.value
                |> Placeholder
                |> List.Nonempty.fromElement


invalidRecordNames =
    Set.fromList [ "en", "fr", "it", "dk", "da", "de", "sv", "es", "nl" ]


getLanguageShortName : String -> Maybe String
getLanguageShortName functionName =
    let
        toLower =
            String.toLower functionName

        contains text =
            String.contains text toLower
    in
    if contains "english" then
        Just "en"

    else if contains "swedish" || contains "sweden" then
        Just "sv"

    else if contains "danish" || contains "denmark" then
        Just "da"

    else if contains "french" || contains "france" then
        Just "fr"

    else if contains "italian" || contains "italy" then
        Just "it"

    else if contains "german" || contains "germany" then
        Just "de"

    else if contains "spanish" || contains "spain" then
        Just "es"

    else if contains "dutch" || contains "holland" || contains "netherlands" then
        Just "nl"

    else
        Nothing


getLanguageLongName : String -> Maybe String
getLanguageLongName functionName =
    let
        toLower =
            String.toLower functionName

        contains text =
            String.contains text toLower
    in
    if contains "english" then
        Just "english"

    else if contains "swedish" || contains "sweden" then
        Just "swedish"

    else if contains "danish" || contains "denmark" then
        Just "danish"

    else if contains "french" || contains "france" then
        Just "french"

    else if contains "italian" || contains "italy" then
        Just "italian"

    else if contains "german" || contains "germany" then
        Just "german"

    else if contains "spanish" || contains "spain" then
        Just "spanish"

    else if contains "dutch" || contains "holland" || contains "netherlands" then
        Just "dutch"

    else
        Nothing


parseRecordField : Node RecordSetter -> Translation
parseRecordField =
    \(Node _ ( Node _ name, Node range value )) ->
        if Set.member name invalidRecordNames then
            { name = name
            , rowNumber = range.start.row
            , expression = value
            }
                |> ParseError

        else
            case value of
                LambdaExpression { args, expression } ->
                    { name = name
                    , parameters = List.map Node.value args
                    , value = parseTranslationValue expression |> Node (Node.range expression)
                    }
                        |> Translation

                Literal text ->
                    { name = name
                    , parameters = []
                    , value = List.Nonempty.fromElement (TextContent text) |> Node range
                    }
                        |> Translation

                RecordExpr record ->
                    { name = name
                    , translations = List.map parseRecordField record
                    }
                        |> Group

                _ ->
                    { name = name
                    , rowNumber = range.start.row
                    , expression = value
                    }
                        |> ParseError


isTranslationRecord : Nonempty Translation -> Bool
isTranslationRecord translations =
    let
        isTranslationRecordHelper list =
            List.foldl
                (\a ( valid, total ) ->
                    case a of
                        ParseError _ ->
                            ( valid, total + 1 )

                        Group group ->
                            isTranslationRecordHelper group.translations

                        Translation _ ->
                            ( valid + 1, total + 1 )
                )
                ( 0, 0 )
                list

        ( allValid, allTotal ) =
            List.Nonempty.toList translations |> isTranslationRecordHelper
    in
    if allTotal <= 0 then
        False

    else
        toFloat allValid / toFloat allTotal > 0.5


translationToDict :
    List String
    -> List Translation
    -> Dict (Nonempty String) (Result () { value : Nonempty Content, range : Range })
translationToDict path translations =
    List.foldl
        (\translation dict ->
            case translation of
                Translation translationValue ->
                    Dict.insert
                        (Nonempty translationValue.name path |> List.Nonempty.reverse)
                        (Ok
                            { value = Node.value translationValue.value
                            , range = Node.range translationValue.value
                            }
                        )
                        dict

                ParseError error ->
                    Dict.insert
                        (Nonempty error.name path |> List.Nonempty.reverse)
                        (Err ())
                        dict

                Group group ->
                    translationToDict (group.name :: path) group.translations |> Dict.union dict
        )
        Dict.empty
        translations


parse : String -> String -> Result (List DeadEnd) (List TranslationDeclaration)
parse modulePath moduleCode =
    case Elm.Parser.parse moduleCode of
        Ok rawFile ->
            let
                file : File
                file =
                    Elm.Processing.process Elm.Processing.init rawFile
            in
            file.declarations
                |> List.filterMap
                    (\(Node _ declaration) ->
                        case declaration of
                            FunctionDeclaration function ->
                                let
                                    functionDeclaration =
                                        Node.value function.declaration
                                in
                                case
                                    ( getLanguageShortName (Node.value functionDeclaration.name)
                                    , Node.value functionDeclaration.expression
                                    )
                                of
                                    ( Just language, RecordExpr record ) ->
                                        case List.map parseRecordField record |> List.Nonempty.fromList of
                                            Just translations ->
                                                if isTranslationRecord translations then
                                                    { filePath = modulePath
                                                    , documentation = function.documentation
                                                    , signature = function.signature
                                                    , functionName = Node.value functionDeclaration.name
                                                    , parameters = List.map Node.value functionDeclaration.arguments
                                                    , language = language
                                                    , translations = translationToDict [] (List.Nonempty.toList translations)
                                                    }
                                                        |> Just

                                                else
                                                    Nothing

                                            Nothing ->
                                                Nothing

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Ok

        Err error ->
            Err error
