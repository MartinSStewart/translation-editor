module Evergreen.V16.Types exposing (..)

import AssocList
import Browser
import Browser.Navigation
import Bytes
import Evergreen.V16.Github
import Evergreen.V16.TranslationParser
import Http
import Lamdera
import List.Nonempty
import Set
import Url


type alias StartModel =
    { personalAccessToken : String
    , pressedSubmit : Bool
    , loginFailed : Bool
    }


type alias LoadingModel =
    { oauthToken : Evergreen.V16.Github.OAuthToken
    , filesRemaining : List ( String, Url.Url )
    , directoriesRemaining : Set.Set String
    , fileContents : List ( String, String )
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
            , result : List Evergreen.V16.TranslationParser.TranslationDeclaration
            , original : String
            }
    , oauthToken : Evergreen.V16.Github.OAuthToken
    , loadedChanges : AssocList.Dict TranslationId String
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


type alias TranslationGroup =
    { path : List.Nonempty.Nonempty String
    , ids :
        List.Nonempty.Nonempty
            { filePath : String
            , functionName : String
            }
    }


type alias EditorModel =
    { files :
        AssocList.Dict
            String
            { original : String
            }
    , translations : List Evergreen.V16.TranslationParser.TranslationDeclaration
    , oauthToken : Evergreen.V16.Github.OAuthToken
    , changes : AssocList.Dict TranslationId String
    , submitStatus : SubmitStatus
    , pullRequestMessage : String
    , hiddenLanguages : Set.Set String
    , changeCounter : Int
    , allLanguages : Set.Set String
    , groups : List TranslationGroup
    }


type State
    = Start StartModel
    | Authenticate
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
    | GotRepository Evergreen.V16.Github.OAuthToken (Result Http.Error (List ( String, String )))
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
        , result : Result () (List Evergreen.V16.TranslationParser.TranslationDeclaration)
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
    | DebounceFinished
        { changeCounter : Int
        }
    | PressedResetTranslationGroup
        { path : List.Nonempty.Nonempty String
        }


type ToBackend
    = AuthenticateRequest Evergreen.V16.Github.OAuthCode
    | GetZipRequest Evergreen.V16.Github.OAuthToken


type BackendMsg
    = GotAccessToken Lamdera.ClientId (Result Http.Error Evergreen.V16.Github.AccessTokenResponse)
    | GotRepositoryBackend
        (Result
            Http.Error
            { defaultBranch : String
            }
        )
    | LoadedZipBackend Lamdera.ClientId (Result Http.Error Bytes.Bytes)


type ToFrontend
    = AuthenticateResponse (Result Http.Error Evergreen.V16.Github.OAuthToken)
    | GetZipResponse (Result Http.Error Bytes.Bytes)
