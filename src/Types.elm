module Types exposing (..)

import AssocList exposing (Dict)
import Browser exposing (UrlRequest)
import Browser.Navigation
import Bytes exposing (Bytes)
import Cache exposing (Cache)
import Github exposing (AccessTokenResponse, Branch, OAuthCode, OAuthToken)
import Http
import Lamdera exposing (ClientId)
import List.Nonempty exposing (Nonempty)
import Set exposing (Set)
import TranslationParser exposing (TranslationDeclaration)
import Url exposing (Url)


type alias FrontendModel =
    { windowWidth : Int
    , windowHeight : Int
    , navKey : Browser.Navigation.Key
    , state : State
    , dummyChange : ()
    }


type State
    = Start StartModel
    | Authenticate (Maybe Cache) (Maybe Branch)
    | Loading LoadingModel
    | Parsing ParsingModel
    | Editor EditorModel
    | ParsingFailed { path : String, branch : Branch }
    | LoadFailed Http.Error


type alias LoadingModel =
    { oauthToken : OAuthToken
    , filesRemaining : List ( String, Url )
    , directoriesRemaining : Set String
    , fileContents : List ( String, String )
    , cache : Maybe Cache
    , branch : Maybe Branch
    }


type alias ParsingModel =
    { unparsedFiles : List ( String, String )
    , parsedFiles : List { path : String, result : List TranslationDeclaration, original : String }
    , oauthToken : OAuthToken
    , loadedChanges : Dict TranslationId String
    , cache : Maybe Cache
    , branch : Branch
    }


type alias TranslationGroup =
    { path : Nonempty String
    , filePath : String
    , functionNames : Nonempty String
    , isMarkdown : IsMarkdown
    }


type IsMarkdown
    = IsMarkdown
    | IsPartiallyMarkdown
    | IsPlainText


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
    , branch : Branch

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
    , cache : Maybe Cache
    , branch : Maybe Branch
    }


type alias BackendModel =
    {}


type FrontendMsg
    = PressedLink UrlRequest
    | UrlChanged
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
    | DummyChange


type ToBackend
    = AuthenticateRequest OAuthCode
    | GetZipRequest OAuthToken (Maybe Branch)


type BackendMsg
    = GotAccessToken ClientId (Result Http.Error AccessTokenResponse)
    | LoadedZipBackend ClientId (Result Http.Error ( Branch, Bytes ))


type ToFrontend
    = AuthenticateResponse (Result Http.Error OAuthToken)
    | GetZipResponse (Result Http.Error ( Branch, Bytes ))
