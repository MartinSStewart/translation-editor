module Cache exposing (Cache, CachedFile, CachedTranslation, Error(..), cacheFiles, cachedFileCodec, cachedTranslationCodec, codec, hashKey, loadTranslation, nonemptyCodec)

import AssocList as Dict exposing (Dict)
import AstCodec
import Dict as RegularDict
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


cacheFiles : List { originalCode : String, translations : List TranslationDeclaration } -> Cache
cacheFiles files =
    List.map
        (\{ originalCode, translations } ->
            ( Murmur3.hashString hashKey originalCode, List.map cacheTranslation translations )
        )
        files
        |> List.sortBy Tuple.first
        -- We only cache up to 1000 files so that the cache doesn't grow without bound
        |> List.take 1000
        |> RegularDict.fromList


hashKey =
    123


type alias Cache =
    RegularDict.Dict Int (List CachedTranslation)


codec : Serialize.Codec Error Cache
codec =
    Serialize.dict Serialize.int (Serialize.list cachedTranslationCodec)


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


type alias FileHash =
    Int


type alias CachedFile =
    { fileHash : FileHash
    , translations : List CachedTranslation
    }


cachedFileCodec : Serialize.Codec Error CachedFile
cachedFileCodec =
    Serialize.record CachedFile
        |> Serialize.field .fileHash Serialize.int
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


nonemptyCodec : Serialize.Codec Error a -> Serialize.Codec Error (Nonempty a)
nonemptyCodec codec_ =
    Serialize.list codec_
        |> Serialize.mapValid (List.Nonempty.fromList >> Result.fromMaybe NonemptyListCannotBeEmpty) List.Nonempty.toList
