module Evergreen.V1.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Bytes
import Evergreen.V1.Cache
import Evergreen.V1.Github
import Evergreen.V1.TranslationParser
import Http
import Lamdera
import List.Nonempty
import Set
import Url


type alias StartModel =
    { personalAccessToken : String
    , pressedSubmit : Bool
    , loginFailed : Bool
    , cache : Maybe Evergreen.V1.Cache.Cache
    , branch : Maybe Evergreen.V1.Github.Branch
    }


type alias LoadingModel =
    { oauthToken : Evergreen.V1.Github.OAuthToken
    , filesRemaining : List ( String, Url.Url )
    , directoriesRemaining : Set.Set String
    , fileContents : List ( String, String )
    , cache : Maybe Evergreen.V1.Cache.Cache
    , branch : Maybe Evergreen.V1.Github.Branch
    }


type alias TranslationId =
    { filePath : String
    , functionName : String
    , path : List.Nonempty.Nonempty String
    }


type alias ParsingModel =
    { unparsedFiles : List ( String, String )
    , parsedFiles :
        List
            { path : String
            , result : List Evergreen.V1.TranslationParser.TranslationDeclaration
            , original : String
            }
    , oauthToken : Evergreen.V1.Github.OAuthToken
    , loadedChanges : AssocList.Dict TranslationId String
    , cache : Maybe Evergreen.V1.Cache.Cache
    , branch : Evergreen.V1.Github.Branch
    }


type SubmitStatus
    = NotSubmitted
        { pressedSubmit : Bool
        }
    | SubmitConfirm
        (List.Nonempty.Nonempty
            { path : String
            , content : String
            }
        )
    | Submitting
    | SubmitSuccessful
        { apiUrl : String
        , htmlUrl : String
        }
    | SubmitFailed
        { pressedSubmit : Bool
        , error : ( String, Http.Error )
        }


type IsMarkdown
    = IsMarkdown
    | IsPartiallyMarkdown
    | IsPlainText


type alias TranslationGroup =
    { path : List.Nonempty.Nonempty String
    , filePath : String
    , functionNames : List.Nonempty.Nonempty String
    , isMarkdown : IsMarkdown
    }


type alias EditorModel =
    { files :
        AssocList.Dict
            String
            { original : String
            }
    , translations : List Evergreen.V1.TranslationParser.TranslationDeclaration
    , oauthToken : Evergreen.V1.Github.OAuthToken
    , changes : AssocList.Dict TranslationId String
    , submitStatus : SubmitStatus
    , pullRequestMessage : String
    , hiddenLanguages : Set.Set String
    , changeCounter : Int
    , showOnlyMissingTranslations : Bool
    , name : String
    , branch : Evergreen.V1.Github.Branch
    , allLanguages : Set.Set String
    , groups : List TranslationGroup
    }


type State
    = Start StartModel
    | Authenticate (Maybe Evergreen.V1.Cache.Cache) (Maybe Evergreen.V1.Github.Branch)
    | Loading LoadingModel
    | Parsing ParsingModel
    | Editor EditorModel
    | ParsingFailed
        { path : String
        , branch : Evergreen.V1.Github.Branch
        }
    | LoadFailed Http.Error


type alias FrontendModel =
    { windowWidth : Int
    , windowHeight : Int
    , navKey : Browser.Navigation.Key
    , state : State
    , dummyChange : ()
    }


type alias BackendModel =
    {}


type FrontendMsg
    = PressedLink Browser.UrlRequest
    | UrlChanged
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
        , result : Result () (List Evergreen.V1.TranslationParser.TranslationDeclaration)
        , original : String
        }
    | TypedTranslation TranslationId String
    | GotWindowSize Int Int
    | PressedSubmitChanges
    | PressedConfirmSubmitChanges
    | PressedCancelSubmitChanges
    | PullRequestCreated
        (Result
            ( String, Http.Error )
            { apiUrl : String
            , htmlUrl : String
            }
        )
    | PressedShowLanguage String
    | PressedHideLanguage String
    | TypedPullRequestMessage String
    | TypedName String
    | DebounceFinished
        { changeCounter : Int
        }
    | PressedResetTranslationGroup
        { path : List.Nonempty.Nonempty String
        }
    | PressedCloseSubmitSuccessful
    | PressedToggleOnlyMissingTranslations


type ToBackend
    = AuthenticateRequest Evergreen.V1.Github.OAuthCode
    | GetZipRequest Evergreen.V1.Github.OAuthToken (Maybe Evergreen.V1.Github.Branch)


type BackendMsg
    = GotAccessToken Lamdera.ClientId (Result Http.Error Evergreen.V1.Github.AccessTokenResponse)
    | LoadedZipBackend Lamdera.ClientId (Result Http.Error ( Evergreen.V1.Github.Branch, Bytes.Bytes ))


type ToFrontend
    = AuthenticateResponse (Result Http.Error Evergreen.V1.Github.OAuthToken)
    | GetZipResponse (Result Http.Error ( Evergreen.V1.Github.Branch, Bytes.Bytes ))
