module Cache exposing (Cache, CachedTranslation, Error(..), cacheFiles, codec, hashKey, loadTranslation, nonemptyCodec)

import AssocList as Dict exposing (Dict)
import AstCodec
import Dict as RegularDict
import List.Nonempty exposing (Nonempty)
import Murmur3
import Serialize
import TranslationParser exposing (Content, TranslationDeclaration, TranslationValue_)


type Error
    = NonemptyListCannotBeEmpty
    | InvalidCharInExpression


type alias CachedTranslation =
    { functionName : String
    , translations : Dict (Nonempty String) (Result () TranslationValue_)
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


type CacheVersion
    = CacheVersion2 Cache


codec : Serialize.Codec Error Cache
codec =
    Serialize.customType
        (\a value ->
            case value of
                CacheVersion2 data0 ->
                    a data0
        )
        |> Serialize.variant1 CacheVersion2 cacheCodec
        |> Serialize.finishCustomType
        |> Serialize.map
            (\cacheVersion ->
                case cacheVersion of
                    CacheVersion2 cache ->
                        cache
            )
            CacheVersion2


cacheCodec : Serialize.Codec Error Cache
cacheCodec =
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
                    translationValueCodec
                )
            )
        |> Serialize.finishRecord


translationValueCodec : Serialize.Codec Error TranslationValue_
translationValueCodec =
    Serialize.record TranslationValue_
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
        |> Serialize.field .isMarkdown
            (Serialize.maybe AstCodec.expression
                |> Serialize.mapError
                    (\error ->
                        case error of
                            AstCodec.InvalidChar ->
                                InvalidCharInExpression
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
