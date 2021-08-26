module Types exposing (..)

import AssocList exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Bytes exposing (Bytes)
import Github exposing (AccessTokenResponse, OAuthCode, OAuthToken, Scope)
import Http
import Lamdera exposing (ClientId)
import List.Nonempty exposing (Nonempty)
import Set exposing (Set)
import Time
import TranslationParser exposing (TranslationDeclaration)
import Url exposing (Url)


type alias FrontendModel =
    { windowWidth : Int
    , windowHeight : Int
    , navKey : Browser.Navigation.Key
    , state : State
    }


type State
    = Start StartModel
    | Authenticate
    | Loading LoadingModel
    | Parsing ParsingModel
    | Editor EditorModel
    | ParsingFailed { path : String }
    | LoadFailed Http.Error


type alias LoadingModel =
    { oauthToken : OAuthToken
    , filesRemaining : List ( String, Url )
    , directoriesRemaining : Set String
    , fileContents : List ( String, String )
    }


type alias ParsingModel =
    { unparsedFiles : List ( String, String )
    , parsedFiles : List { path : String, result : List TranslationDeclaration, original : String }
    , oauthToken : OAuthToken
    , loadedChanges : Dict TranslationId String
    }


type alias TranslationGroup =
    { path : Nonempty String
    , ids : Nonempty { filePath : String, functionName : String }
    }


type alias EditorModel =
    { files : Dict String { original : String }
    , translations : List TranslationDeclaration
    , oauthToken : OAuthToken
    , changes : Dict TranslationId String
    , submitStatus : SubmitStatus
    , pullRequestMessage : String
    , hiddenLanguages : Set String
    , changeCounter : Int
    , showOnlyMissingTranslations : Bool
    , name : String

    -- These fields can be derived from translations but we avoid that for performance
    -- This isn't a big issue as these values should never change
    , allLanguages : Set String
    , groups : List TranslationGroup
    }


type SubmitStatus
    = NotSubmitted { pressedSubmit : Bool }
    | SubmitConfirm (Nonempty { path : String, content : String })
    | Submitting
    | SubmitSuccessful { apiUrl : String, htmlUrl : String }
    | SubmitFailed { pressedSubmit : Bool, error : ( String, Http.Error ) }


type alias TranslationId =
    { filePath : String
    , functionName : String
    , path : Nonempty String
    }


type alias StartModel =
    { personalAccessToken : String
    , pressedSubmit : Bool
    , loginFailed : Bool
    }


type alias BackendModel =
    {}


type FrontendMsg
    = PressedLink UrlRequest
    | UrlChanged Url
    | GotRepository OAuthToken (Result Http.Error (List ( String, String )))
    | TypedPersonalAccessToken String
    | PressedSubmitPersonalAccessToken
    | GotLocalStorageData (Result String { key : String, value : Maybe String })
    | ParsedFile { path : String, result : Result () (List TranslationDeclaration), original : String }
    | TypedTranslation TranslationId String
    | GotWindowSize Int Int
    | PressedSubmitChanges
    | PressedConfirmSubmitChanges
    | PressedCancelSubmitChanges
    | PullRequestCreated (Result ( String, Http.Error ) { apiUrl : String, htmlUrl : String })
    | PressedShowLanguage String
    | PressedHideLanguage String
    | TypedPullRequestMessage String
    | TypedName String
    | DebounceFinished { changeCounter : Int }
    | PressedResetTranslationGroup { path : Nonempty String }
    | PressedCloseSubmitSuccessful
    | PressedToggleOnlyMissingTranslations


type ToBackend
    = AuthenticateRequest OAuthCode
    | GetZipRequest OAuthToken


type BackendMsg
    = GotAccessToken ClientId (Result Http.Error AccessTokenResponse)
    | GotRepositoryBackend (Result Http.Error { defaultBranch : String })
    | LoadedZipBackend ClientId (Result Http.Error Bytes)


type ToFrontend
    = AuthenticateResponse (Result Http.Error OAuthToken)
    | GetZipResponse (Result Http.Error Bytes)
