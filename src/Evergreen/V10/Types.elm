module Evergreen.V10.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Bytes
import Evergreen.V10.Github
import Evergreen.V10.TranslationParser
import Http
import Lamdera
import List.Nonempty
import Set
import Url


type alias StartModel =
    { personalAccessToken : String
    , pressedSubmit : Bool
    }


type alias LoadingModel =
    { oauthToken : Evergreen.V10.Github.OAuthToken
    , filesRemaining : List ( String, Url.Url )
    , directoriesRemaining : Set.Set String
    , fileContents : List ( String, String )
    }


type alias ParsingModel =
    { unparsedFiles : List ( String, String )
    , parsedFiles :
        List
            { path : String
            , result : List Evergreen.V10.TranslationParser.TranslationDeclaration
            , original : String
            }
    , oauthToken : Evergreen.V10.Github.OAuthToken
    }


type alias TranslationId =
    { filePath : String
    , functionName : String
    , path : List.Nonempty.Nonempty String
    }


type SubmitStatus
    = NotSubmitted
        { pressedSubmit : Bool
        }
    | Submitting
    | SubmitSuccessful
        { apiUrl : String
        , htmlUrl : String
        }
    | SubmitFailed
        { pressedSubmit : Bool
        , error : ( String, Http.Error )
        }


type alias EditorModel =
    { files :
        AssocList.Dict
            String
            { original : String
            }
    , translations : List Evergreen.V10.TranslationParser.TranslationDeclaration
    , oauthToken : Evergreen.V10.Github.OAuthToken
    , changes : AssocList.Dict TranslationId String
    , submitStatus : SubmitStatus
    }


type State
    = Start StartModel
    | Loading LoadingModel
    | Parsing ParsingModel
    | Editor EditorModel
    | ParsingFailed
        { path : String
        }
    | LoadFailed Http.Error


type alias FrontendModel =
    { windowWidth : Int
    , windowHeight : Int
    , navKey : Browser.Navigation.Key
    , state : State
    }


type alias BackendModel =
    {}


type FrontendMsg
    = PressedLink Browser.UrlRequest
    | UrlChanged Url.Url
    | GotRepository Evergreen.V10.Github.OAuthToken (Result Http.Error (List ( String, String )))
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
        , result : Result () (List Evergreen.V10.TranslationParser.TranslationDeclaration)
        , original : String
        }
    | TypedTranslation TranslationId String
    | GotWindowSize Int Int
    | PressedSubmitChanges
    | PullRequestCreated
        (Result
            ( String, Http.Error )
            { apiUrl : String
            , htmlUrl : String
            }
        )


type ToBackend
    = AuthenticateRequest Evergreen.V10.Github.OAuthCode
    | GetZipRequest Evergreen.V10.Github.OAuthToken


type BackendMsg
    = GotAccessToken Lamdera.ClientId (Result Http.Error Evergreen.V10.Github.AccessTokenResponse)
    | GotRepositoryBackend
        (Result
            Http.Error
            { defaultBranch : String
            }
        )
    | LoadedZipBackend Lamdera.ClientId (Result Http.Error Bytes.Bytes)


type ToFrontend
    = AuthenticateResponse (Result Http.Error Evergreen.V10.Github.OAuthToken)
    | GetZipResponse (Result Http.Error Bytes.Bytes)
