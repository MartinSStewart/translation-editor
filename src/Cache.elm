module Cache exposing (CachedFile, CachedTranslation, Error(..), cacheFile, cachedFileCodec, nonemptyCodec)

import AssocList as Dict exposing (Dict)
import AstCodec
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty)
import Murmur3
import Serialize
import TranslationParser exposing (Content, TranslationDeclaration)


type Error
    = NonemptyListCannotBeEmpty
    | InvalidCharInExpression


type alias CachedTranslation =
    { functionName : String
    , translations : Dict (Nonempty String) (Result () { value : Nonempty Content, range : Range })
    }


cacheFile : { originalCode : String, translations : List TranslationDeclaration } -> CachedFile
cacheFile { originalCode, translations } =
    { fileHash = Murmur3.hashString 123 originalCode |> FileHash
    , translations = List.map cacheTranslation translations
    }


cacheTranslation : TranslationDeclaration -> CachedTranslation
cacheTranslation translation =
    { functionName = translation.functionName
    , translations = translation.translations
    }


loadTranslation : { filePath : String, cachedTranslation : CachedTranslation } -> Maybe TranslationDeclaration
loadTranslation { filePath, cachedTranslation } =
    case TranslationParser.getLanguageShortName cachedTranslation.functionName of
        Just language ->
            { filePath = filePath
            , functionName = cachedTranslation.functionName
            , language = language
            , translations = cachedTranslation.translations
            }
                |> Just

        Nothing ->
            Nothing


type FileHash
    = FileHash Int


type alias CachedFile =
    { fileHash : FileHash
    , translations : List CachedTranslation
    }


cachedFileCodec : Serialize.Codec Error CachedFile
cachedFileCodec =
    Serialize.record CachedFile
        |> Serialize.field .fileHash fileHashCodec
        |> Serialize.field .translations (Serialize.list cachedTranslationCodec)
        |> Serialize.finishRecord


cachedTranslationCodec : Serialize.Codec Error CachedTranslation
cachedTranslationCodec =
    Serialize.record CachedTranslation
        |> Serialize.field .functionName Serialize.string
        |> Serialize.field
            .translations
            (dict
                (nonemptyCodec Serialize.string)
                (Serialize.result
                    Serialize.unit
                    (Serialize.record (\a b -> { value = a, range = b })
                        |> Serialize.field
                            .value
                            (nonemptyCodec
                                (TranslationParser.contentCodec
                                    |> Serialize.mapError
                                        (\error ->
                                            case error of
                                                AstCodec.InvalidChar ->
                                                    InvalidCharInExpression
                                        )
                                )
                            )
                        |> Serialize.field .range AstCodec.range
                        |> Serialize.finishRecord
                    )
                )
            )
        |> Serialize.finishRecord


dict : Serialize.Codec e k -> Serialize.Codec e v -> Serialize.Codec e (Dict k v)
dict keyCodec valueCodec =
    Serialize.list (Serialize.tuple keyCodec valueCodec)
        |> Serialize.map Dict.fromList Dict.toList


fileHashCodec : Serialize.Codec e FileHash
fileHashCodec =
    Serialize.customType
        (\a value ->
            case value of
                FileHash data0 ->
                    a data0
        )
        |> Serialize.variant1 FileHash Serialize.int
        |> Serialize.finishCustomType


nonemptyCodec : Serialize.Codec Error a -> Serialize.Codec Error (Nonempty a)
nonemptyCodec codec =
    Serialize.list codec
        |> Serialize.mapValid (List.Nonempty.fromList >> Result.fromMaybe NonemptyListCannotBeEmpty) List.Nonempty.toList
