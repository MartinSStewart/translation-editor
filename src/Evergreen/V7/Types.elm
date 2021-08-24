module Evergreen.V7.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Bytes
import Evergreen.V7.Editor
import Evergreen.V7.Github
import Evergreen.V7.TranslationParser
import Http
import Lamdera
import Set
import Url


type alias StartModel =
    { navKey : Browser.Navigation.Key
    , personalAccessToken : String
    , pressedSubmit : Bool
    }


type alias LoadingModel =
    { navKey : Browser.Navigation.Key
    , oauthToken : Evergreen.V7.Github.OAuthToken
    , filesRemaining : List ( String, Url.Url )
    , directoriesRemaining : Set.Set String
    , fileContents : List ( String, String )
    }


type alias ParsingModel =
    { navKey : Browser.Navigation.Key
    , unparsedFiles : List ( String, String )
    , parsedFiles :
        List
            { path : String
            , result : List Evergreen.V7.TranslationParser.TranslationDeclaration
            , original : String
            }
    , oauthToken : Evergreen.V7.Github.OAuthToken
    }


type alias ParsingFinishedModel =
    { navKey : Browser.Navigation.Key
    , files :
        List
            { path : String
            , result : List Evergreen.V7.TranslationParser.TranslationDeclaration
            , original : String
            }
    , oauthToken : Evergreen.V7.Github.OAuthToken
    , changes : AssocList.Dict Evergreen.V7.Editor.TranslationId String
    }


type FrontendModel
    = Start StartModel
    | Loading LoadingModel
    | Parsing ParsingModel
    | ParsingFinished ParsingFinishedModel
    | ParsingFailed
        { navKey : Browser.Navigation.Key
        , path : String
        }
    | LoadFailed Browser.Navigation.Key Http.Error


type alias BackendModel =
    {}


type FrontendMsg
    = PressedLink Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRepository Evergreen.V7.Github.OAuthToken (Result Http.Error (List ( String, String )))
    | TypedPersonalAccessToken String
    | PressedSubmitPersonalAccessToken
    | GotLocalStorageData
        (Result
            String
            { key : String
            , value : Maybe String
            }
        )
    | ParsedFile
        { path : String
        , result : Result () (List Evergreen.V7.TranslationParser.TranslationDeclaration)
        , original : String
        }
    | LoadedPaths
        String
        (Result
            Http.Error
            { filePaths : List ( String, Url.Url )
            , directoryPaths : Set.Set String
            }
        )
    | LoadedFileContent (Result Http.Error ( String, String ))
    | TypedTranslation Evergreen.V7.Editor.TranslationId String


type ToBackend
    = AuthenticateRequest Evergreen.V7.Github.OAuthCode
    | GetZipRequest Evergreen.V7.Github.OAuthToken


type BackendMsg
    = GotAccessToken Lamdera.ClientId (Result Http.Error Evergreen.V7.Github.AccessTokenResponse)
    | GotRepositoryBackend
        (Result
            Http.Error
            { defaultBranch : String
            }
        )
    | LoadedZipBackend Lamdera.ClientId (Result Http.Error Bytes.Bytes)


type ToFrontend
    = AuthenticateResponse (Result Http.Error Evergreen.V7.Github.OAuthToken)
    | GetZipResponse (Result Http.Error Bytes.Bytes)
