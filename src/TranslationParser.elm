module TranslationParser exposing
    ( Content(..)
    , Translation(..)
    , TranslationDeclaration
    , TranslationValue
    , TranslationValue_
    , contentCodec
    , contentToString
    , getLanguageLongName
    , getLanguageShortName
    , getPlaceholders
    , namesToPlaceholders
    , parse
    , writeContents
    )

import AssocList as Dict exposing (Dict)
import AstCodec exposing (DecodeError)
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import NodeHelper
import Parser exposing (DeadEnd)
import Serialize
import Set


type alias TranslationDeclaration =
    { filePath : String
    , functionName : String
    , language : String
    , translations : Dict (Nonempty String) (Result () TranslationValue_)
    }


type alias TranslationValue_ =
    { value : Nonempty Content, range : Range, isMarkdown : Maybe Expression }


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


type Translation
    = Translation TranslationValue
    | ParseError { name : String, rowNumber : Int, expression : Expression }
    | Group { name : String, translations : List Translation }


type alias TranslationValue =
    { name : String, parameters : List Pattern, value : Node (Nonempty Content), isMarkdown : Maybe Expression }


type Content
    = TextContent String
    | Placeholder Expression


type alias MarkdownData =
    { placeholder : Expression, markdownFunction : Expression }


contentCodec : Serialize.Codec DecodeError Content
contentCodec =
    Serialize.customType
        (\a b value ->
            case value of
                TextContent data0 ->
                    a data0

                Placeholder data0 ->
                    b data0
        )
        |> Serialize.variant1 TextContent Serialize.string
        |> Serialize.variant1 Placeholder AstCodec.expression
        |> Serialize.finishCustomType


writeContents : Maybe Expression -> Nonempty Content -> Expression
writeContents isMarkdown contents =
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
        |> (\contentExpression ->
                case isMarkdown of
                    Just markdownFunction ->
                        OperatorApplication "|>" Right contentExpression (NodeHelper.node markdownFunction)

                    Nothing ->
                        Node.value contentExpression
           )


parserMarkdownTranslation : Node Expression -> List String -> String -> Maybe { content : Nonempty Content, isMarkdown : Maybe Expression }
parserMarkdownTranslation subexpression moduleName functionOrValue =
    if List.any (String.toLower >> String.contains "markdown") (functionOrValue :: moduleName) then
        { content = parseTranslationValueHelper subexpression
        , isMarkdown = Just (FunctionOrValue moduleName functionOrValue)
        }
            |> Just

    else
        Nothing


parseTranslationValueHelper : Node Expression -> Nonempty Content
parseTranslationValueHelper expression_ =
    case Node.value expression_ of
        ParenthesizedExpression subexpression ->
            parseTranslationValueHelper subexpression

        OperatorApplication "++" _ left right ->
            List.Nonempty.append (parseTranslationValueHelper left) (parseTranslationValueHelper right)

        Literal text ->
            TextContent text |> List.Nonempty.fromElement

        _ ->
            NodeHelper.noRangeExpression expression_
                |> Node.value
                |> Placeholder
                |> List.Nonempty.fromElement


parseTranslationValue : Node Expression -> Maybe { content : Nonempty Content, isMarkdown : Maybe Expression }
parseTranslationValue expression =
    case Node.value expression of
        OperatorApplication "|>" _ left (Node _ (FunctionOrValue moduleName functionOrValue)) ->
            parserMarkdownTranslation left moduleName functionOrValue

        OperatorApplication "<|" _ (Node _ (FunctionOrValue moduleName functionOrValue)) right ->
            parserMarkdownTranslation right moduleName functionOrValue

        Application [ Node _ (FunctionOrValue moduleName functionOrValue), second ] ->
            parserMarkdownTranslation second moduleName functionOrValue

        _ ->
            Just { content = parseTranslationValueHelper expression, isMarkdown = Nothing }


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
parseRecordField (Node _ ( Node _ name, Node range value )) =
    -- We don't want to read in translation records that use the old format.
    if Set.member name invalidRecordNames then
        { name = name
        , rowNumber = range.start.row
        , expression = value
        }
            |> ParseError

    else
        parseRecordFieldHelper range name value


parseRecordFieldHelper : Range -> String -> Expression -> Translation
parseRecordFieldHelper range name value =
    let
        parserMarkdownHelper left moduleName functionOrValue =
            case parserMarkdownTranslation left moduleName functionOrValue of
                Just { content, isMarkdown } ->
                    { name = name
                    , parameters = []
                    , value = Node range content
                    , isMarkdown = isMarkdown
                    }
                        |> Translation

                Nothing ->
                    { name = name
                    , rowNumber = range.start.row
                    , expression = value
                    }
                        |> ParseError
    in
    case value of
        LetExpression { expression } ->
            parseRecordFieldHelper (Node.range expression) name (Node.value expression)

        LambdaExpression { args, expression } ->
            case parseTranslationValue expression of
                Just { content, isMarkdown } ->
                    { name = name
                    , parameters = List.map Node.value args
                    , value = Node (Node.range expression) content
                    , isMarkdown = isMarkdown
                    }
                        |> Translation

                Nothing ->
                    { name = name
                    , rowNumber = range.start.row
                    , expression = value
                    }
                        |> ParseError

        Literal text ->
            { name = name
            , parameters = []
            , value = List.Nonempty.fromElement (TextContent text) |> Node range
            , isMarkdown = Nothing
            }
                |> Translation

        RecordExpr record ->
            { name = name
            , translations = List.map parseRecordField record
            }
                |> Group

        OperatorApplication "|>" _ left (Node _ (FunctionOrValue moduleName functionOrValue)) ->
            parserMarkdownHelper left moduleName functionOrValue

        OperatorApplication "<|" _ (Node _ (FunctionOrValue moduleName functionOrValue)) right ->
            parserMarkdownHelper right moduleName functionOrValue

        Application [ Node _ (FunctionOrValue moduleName functionOrValue), second ] ->
            parserMarkdownHelper second moduleName functionOrValue

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
                                |> Tuple.mapBoth ((+) valid) ((+) total)

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
        (toFloat allValid / toFloat allTotal) > 0.5


translationToDict :
    List String
    -> List Translation
    -> Dict (Nonempty String) (Result () { value : Nonempty Content, range : Range, isMarkdown : Maybe Expression })
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
                            , isMarkdown = translationValue.isMarkdown
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

                helper :
                    Elm.Syntax.Expression.FunctionImplementation
                    -> Node Expression
                    -> String
                    -> Maybe TranslationDeclaration
                helper functionDeclaration (Node _ expression) language =
                    case expression of
                        RecordExpr record ->
                            case List.map parseRecordField record |> List.Nonempty.fromList of
                                Just translations ->
                                    if isTranslationRecord translations then
                                        { filePath = modulePath
                                        , functionName = Node.value functionDeclaration.name
                                        , language = language
                                        , translations = translationToDict [] (List.Nonempty.toList translations)
                                        }
                                            |> Just

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                        LetExpression letBlock ->
                            helper functionDeclaration letBlock.expression language

                        _ ->
                            Nothing
            in
            file.declarations
                |> List.filterMap
                    (\(Node _ declaration) ->
                        case declaration of
                            FunctionDeclaration function ->
                                let
                                    functionDeclaration : Elm.Syntax.Expression.FunctionImplementation
                                    functionDeclaration =
                                        Node.value function.declaration
                                in
                                case getLanguageShortName (Node.value functionDeclaration.name) of
                                    Just language ->
                                        helper functionDeclaration functionDeclaration.expression language

                                    Nothing ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Ok

        Err error ->
            Err error
