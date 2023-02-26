port module Frontend exposing (..)

import AssocList as Dict exposing (Dict)
import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Bytes exposing (Bytes)
import Cache exposing (Cache)
import Dict as RegularDict
import Editor
import Element exposing (Element)
import Element.Font
import Element.Input
import Env
import Github exposing (Branch, OAuthCode, Owner)
import Http
import Json.Decode
import Json.Encode
import Lamdera
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Murmur3
import Process
import Serialize
import Set
import String.Nonempty exposing (NonemptyString)
import Task exposing (Task)
import TranslationParser exposing (TranslationDeclaration)
import Types exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query
import Zip
import Zip.Entry


port local_storage_save_to_js : { key : String, value : String } -> Cmd msg


port local_storage_request_load_to_js : { key : String } -> Cmd msg


port local_storage_load_from_js : (Json.Decode.Value -> msg) -> Sub msg


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = PressedLink
        , onUrlChange = \_ -> UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.batch
        [ local_storage_load_from_js
            (Json.Decode.decodeValue loadFromJsDecoder
                >> Result.mapError Json.Decode.errorToString
                >> GotLocalStorageData
            )
        , Browser.Events.onResize GotWindowSize
        ]


loadFromJsDecoder =
    Json.Decode.map2 (\key value -> { key = key, value = value })
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "value" (Json.Decode.nullable Json.Decode.string))


initStart : { a | owner : Maybe Owner, repoName : Maybe String, branch : Maybe Github.Branch } -> StartModel
initStart routeData =
    { personalAccessToken = ""
    , pressedSubmit = False
    , loginFailed = False
    , cache = Nothing
    , githubUrl =
        Maybe.map2 ownerAndRepoNameToUrl routeData.owner routeData.repoName |> Maybe.withDefault ""
    , branch = routeData.branch
    }


init : Url -> Browser.Navigation.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        ( state, cmd ) =
            case Url.Parser.parse parseRoute url of
                Just routeData ->
                    case ( routeData.code, routeData.owner, routeData.repoName ) of
                        ( Just token, Just owner, Just repoName ) ->
                            ( Authenticate
                                { cache = Nothing
                                , owner = owner
                                , repoName = repoName
                                , branch = routeData.branch
                                }
                            , Cmd.batch
                                [ Browser.Navigation.replaceUrl key Env.domain
                                , Lamdera.sendToBackend (AuthenticateRequest token)
                                ]
                            )

                        _ ->
                            ( Start (initStart routeData)
                            , local_storage_request_load_to_js { key = authTokenLocalStorageKey }
                            )

                Nothing ->
                    ( Start (initStart { owner = Nothing, repoName = Nothing, branch = Nothing })
                    , local_storage_request_load_to_js { key = authTokenLocalStorageKey }
                    )
    in
    ( { windowWidth = 1920
      , windowHeight = 1080
      , navKey = key
      , state = state
      , dummyChange = ()
      }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform (\{ viewport } -> GotWindowSize (round viewport.width) (round viewport.height))
        , cmd
        , local_storage_request_load_to_js { key = localStorageCacheKey }
        ]
    )


parseFiles : Browser.Navigation.Key -> ParsingModel -> ( State, Cmd FrontendMsg )
parseFiles navigationKey parsingModel =
    case parsingModel.unparsedFiles of
        ( path, head ) :: rest ->
            ( Parsing { parsingModel | unparsedFiles = rest }
            , Process.sleep 0
                |> Task.andThen
                    (\() ->
                        let
                            hash : Int
                            hash =
                                Murmur3.hashString Cache.hashKey head

                            cachedTranslations : Maybe (List Cache.CachedTranslation)
                            cachedTranslations =
                                Maybe.andThen (RegularDict.get hash) parsingModel.cache
                        in
                        case cachedTranslations of
                            Just translations ->
                                List.filterMap
                                    (\translation ->
                                        Cache.loadTranslation { filePath = path, cachedTranslation = translation }
                                    )
                                    translations
                                    |> Task.succeed

                            Nothing ->
                                case TranslationParser.parse path head of
                                    Ok translations ->
                                        Task.succeed translations

                                    Err _ ->
                                        Task.fail ()
                    )
                |> Task.attempt (\result -> ParsedFile { path = path, result = result, original = head })
            )

        [] ->
            initEditor navigationKey parsingModel


initEditor :
    Browser.Navigation.Key
    ->
        { a
            | parsedFiles : List { path : String, result : List TranslationDeclaration, original : String }
            , oauthToken : Github.OAuthToken
            , loadedChanges : Dict TranslationId String
            , owner : Owner
            , repoName : String
            , branch : Github.Branch
        }
    -> ( State, Cmd msg )
initEditor navigationKey parsingModel =
    let
        translations : List TranslationDeclaration
        translations =
            List.concatMap .result parsingModel.parsedFiles

        files : Dict String { original : String }
        files =
            List.map
                (\file -> ( file.path, { original = file.original } ))
                parsingModel.parsedFiles
                |> Dict.fromList

        allLanguages =
            List.map .language translations |> Set.fromList
    in
    ( Dict.foldl
        (\k v s -> updateChanges k v s |> Tuple.first)
        { submitStatus = NotSubmitted { pressedSubmit = False }
        , files = files
        , translations = translations
        , groups = groupTranslations translations
        , oauthToken = parsingModel.oauthToken
        , changes = Dict.empty
        , changeCounter = 0
        , showOnlyMissingTranslations = False
        , pullRequestMessage = ""
        , name = ""
        , hiddenLanguages = Set.fromList [ "sv" ] |> Set.intersect allLanguages
        , allLanguages = allLanguages
        , owner = parsingModel.owner
        , repoName = parsingModel.repoName
        , branch = parsingModel.branch
        }
        parsingModel.loadedChanges
        |> Editor
    , Cmd.batch
        [ local_storage_save_to_js
            { key = localStorageCacheKey
            , value =
                parsingModel.parsedFiles
                    |> List.map
                        (\parsedFiles ->
                            { originalCode = parsedFiles.original, translations = parsedFiles.result }
                        )
                    |> Cache.cacheFiles
                    |> Serialize.encodeToJson Cache.codec
                    |> Json.Encode.encode 0
            }
        , Browser.Navigation.pushUrl
            navigationKey
            (encodeRoute
                { owner = Just parsingModel.owner
                , repoName = Just parsingModel.repoName
                , branch = Just parsingModel.branch
                }
            )
        ]
    )


encodeRoute : { owner : Maybe Owner, repoName : Maybe String, branch : Maybe Github.Branch } -> String
encodeRoute routeData =
    Url.Builder.absolute
        []
        ((case routeData.owner of
            Just owner ->
                [ Url.Builder.string ownerParameterName (Github.ownerToString owner) ]

            Nothing ->
                []
         )
            ++ (case routeData.repoName of
                    Just repoName ->
                        [ Url.Builder.string repoNameParameterName repoName ]

                    Nothing ->
                        []
               )
            ++ (case routeData.branch of
                    Just branch ->
                        [ Url.Builder.string branchParameterName (Github.branchToString branch) ]

                    Nothing ->
                        []
               )
        )


localStorageCacheKey : String
localStorageCacheKey =
    "cacheKey"


validateGithubUrl : String -> Maybe ( Owner, String )
validateGithubUrl githubUrl =
    case String.trim githubUrl |> Url.fromString of
        Just url ->
            case String.split "/" url.path of
                "" :: owner :: repo :: _ ->
                    Maybe.map2 Tuple.pair (validateOwner owner) (validateRepoName repo)

                _ ->
                    Nothing

        Nothing ->
            Nothing


validateOwner : String -> Maybe Owner
validateOwner ownerText =
    case String.trim ownerText of
        "" ->
            Nothing

        text ->
            Just (Github.owner text)


validateRepoName : String -> Maybe String
validateRepoName repoName =
    case String.trim repoName of
        "" ->
            Nothing

        text ->
            Just text


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        PressedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged ->
            ( model, Cmd.none )

        TypedPersonalAccessToken text ->
            case model.state of
                Start startModel ->
                    ( { model | state = Start { startModel | personalAccessToken = text } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedSubmitPersonalAccessToken ->
            case model.state of
                Start startModel ->
                    case
                        ( validateGithubUrl startModel.githubUrl
                        , String.Nonempty.fromString startModel.personalAccessToken
                        )
                    of
                        ( Just ( owner, repoName ), Just token ) ->
                            let
                                authToken =
                                    String.Nonempty.toString token |> Github.oauthToken
                            in
                            ( { model
                                | state =
                                    Loading
                                        { filesRemaining = []
                                        , directoriesRemaining = Set.empty
                                        , oauthToken = authToken
                                        , fileContents = []
                                        , cache = Nothing
                                        , owner = owner
                                        , repoName = repoName
                                        , branch = startModel.branch
                                        }
                              }
                            , Cmd.batch
                                [ Lamdera.sendToBackend
                                    (GetZipRequest
                                        { owner = owner
                                        , repoName = repoName
                                        , token = authToken
                                        , branch = startModel.branch
                                        }
                                    )
                                ]
                            )

                        _ ->
                            ( { model | state = Start { startModel | pressedSubmit = True } }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        GotLocalStorageData (Ok { key, value }) ->
            if key == authTokenLocalStorageKey then
                case ( model.state, value ) of
                    ( Start startModel, Just oauthToken ) ->
                        case validateGithubUrl startModel.githubUrl of
                            Just ( owner, repoName ) ->
                                startLoading
                                    (Github.oauthToken oauthToken)
                                    startModel.cache
                                    owner
                                    repoName
                                    startModel.branch
                                    model

                            Nothing ->
                                ( { model | state = Start startModel }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else if key == localStorageCacheKey then
                ( case value of
                    Just text ->
                        case
                            Json.Decode.decodeString Json.Decode.value text
                                |> Result.withDefault (Json.Encode.object [])
                                |> Serialize.decodeFromJson Cache.codec
                        of
                            Ok cache ->
                                { model
                                    | state =
                                        case model.state of
                                            Start startModel ->
                                                Start { startModel | cache = Just cache }

                                            Authenticate authenticate ->
                                                Authenticate
                                                    { cache = Just cache
                                                    , owner = authenticate.owner
                                                    , repoName = authenticate.repoName
                                                    , branch = authenticate.branch
                                                    }

                                            Loading loadingModel ->
                                                Loading { loadingModel | cache = Just cache }

                                            Parsing parsingModel ->
                                                Parsing { parsingModel | cache = Just cache }

                                            Editor _ ->
                                                model.state

                                            ParsingFailed _ ->
                                                model.state

                                            LoadFailed _ ->
                                                model.state
                                }

                            Err _ ->
                                model

                    Nothing ->
                        model
                , Cmd.none
                )

            else if key == changesLocalStorageKey then
                case ( model.state, value ) of
                    ( Parsing parsingModel, Just jsonText ) ->
                        ( { model
                            | state =
                                case Json.Decode.decodeString Json.Decode.value jsonText of
                                    Ok json ->
                                        case Serialize.decodeFromJson changesCodec json of
                                            Ok changes ->
                                                Parsing { parsingModel | loadedChanges = changes }

                                            Err _ ->
                                                Parsing parsingModel

                                    Err _ ->
                                        Parsing parsingModel
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        GotLocalStorageData (Err _) ->
            ( model, Cmd.none )

        ParsedFile translation ->
            case model.state of
                Parsing parsingModel ->
                    case translation.result of
                        Ok parsedFile ->
                            parseFiles
                                model.navKey
                                { parsingModel
                                    | parsedFiles =
                                        { path = translation.path
                                        , result = parsedFile
                                        , original = translation.original
                                        }
                                            :: parsingModel.parsedFiles
                                }
                                |> Tuple.mapFirst (\newState -> { model | state = newState })

                        Err _ ->
                            ( { model | state = ParsingFailed { path = translation.path, branch = parsingModel.branch } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TypedTranslation translationId text ->
            case model.state of
                Editor editorModel ->
                    updateChanges translationId text editorModel
                        |> Tuple.mapFirst (\a -> { model | state = Editor a })

                _ ->
                    ( model, Cmd.none )

        GotWindowSize width height ->
            ( { model | windowWidth = width, windowHeight = height }, Cmd.none )

        PressedSubmitChanges ->
            case model.state of
                Editor editorModel ->
                    case ( editorModel.submitStatus, Editor.applyChanges editorModel ) of
                        ( Submitting, _ ) ->
                            ( model, Cmd.none )

                        ( SubmitSuccessful _, _ ) ->
                            ( model, Cmd.none )

                        ( SubmitConfirm _, _ ) ->
                            ( model, Cmd.none )

                        ( _, Ok changes ) ->
                            ( { model
                                | state = Editor { editorModel | submitStatus = SubmitConfirm changes }
                              }
                            , Cmd.none
                            )

                        ( NotSubmitted _, Err () ) ->
                            ( { model
                                | state =
                                    Editor
                                        { editorModel | submitStatus = NotSubmitted { pressedSubmit = True } }
                              }
                            , Cmd.none
                            )

                        ( SubmitFailed submitFailed, Err () ) ->
                            ( { model
                                | state =
                                    Editor
                                        { editorModel
                                            | submitStatus =
                                                SubmitFailed { submitFailed | pressedSubmit = True }
                                        }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        PullRequestCreated result ->
            case model.state of
                Editor editorModel ->
                    ( { model
                        | state =
                            Editor
                                (case result of
                                    Ok ok ->
                                        { editorModel
                                            | submitStatus = SubmitSuccessful ok
                                            , changes = Dict.empty

                                            -- Make sure an autosave doesn't overwrite our reset
                                            , changeCounter = editorModel.changeCounter + 1
                                        }

                                    Err error ->
                                        { editorModel
                                            | submitStatus =
                                                SubmitFailed { pressedSubmit = False, error = error }
                                        }
                                )
                      }
                    , case result of
                        Ok _ ->
                            saveChangesLocally Dict.empty

                        Err _ ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedShowLanguage language ->
            ( case model.state of
                Editor editor ->
                    { model
                        | state =
                            Editor { editor | hiddenLanguages = Set.remove language editor.hiddenLanguages }
                    }

                _ ->
                    model
            , Cmd.none
            )

        PressedHideLanguage language ->
            ( case model.state of
                Editor editor ->
                    { model
                        | state =
                            Editor { editor | hiddenLanguages = Set.insert language editor.hiddenLanguages }
                    }

                _ ->
                    model
            , Cmd.none
            )

        PressedConfirmSubmitChanges ->
            case model.state of
                Editor editor ->
                    case editor.submitStatus of
                        SubmitConfirm changes ->
                            let
                                hash : String
                                hash =
                                    List.Nonempty.foldl
                                        (\{ path, content } state ->
                                            path ++ content ++ state
                                        )
                                        ""
                                        changes
                                        |> Murmur3.hashString 123
                                        |> String.fromInt
                            in
                            ( { model | state = Editor { editor | submitStatus = Submitting } }
                            , createPullRequest
                                editor.oauthToken
                                editor.owner
                                editor.repoName
                                editor.branch
                                (Github.branch ("edit-" ++ hash))
                                changes
                                (pullRequestMessage editor.name editor.pullRequestMessage)
                                |> Task.attempt PullRequestCreated
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressedCancelSubmitChanges ->
            case model.state of
                Editor editor ->
                    case editor.submitStatus of
                        SubmitConfirm _ ->
                            ( { model
                                | state =
                                    Editor { editor | submitStatus = NotSubmitted { pressedSubmit = False } }
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TypedPullRequestMessage message ->
            ( case model.state of
                Editor editor ->
                    { model | state = Editor { editor | pullRequestMessage = message } }

                _ ->
                    model
            , Cmd.none
            )

        TypedName name ->
            ( case model.state of
                Editor editor ->
                    { model | state = Editor { editor | name = name } }

                _ ->
                    model
            , Cmd.none
            )

        DebounceFinished { changeCounter } ->
            case model.state of
                Editor editor ->
                    ( model
                    , if changeCounter == editor.changeCounter then
                        saveChangesLocally editor.changes

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedResetTranslationGroup { path } ->
            case model.state of
                Editor editor ->
                    ( { model
                        | state =
                            Editor
                                { editor
                                    | changes =
                                        Dict.filter
                                            (\translationId _ -> translationId.path /= path)
                                            editor.changes
                                    , changeCounter = editor.changeCounter + 1
                                }
                      }
                    , startDebounce editor
                    )

                _ ->
                    ( model, Cmd.none )

        PressedCloseSubmitSuccessful ->
            case model.state of
                Editor editor ->
                    ( { model
                        | state =
                            Editor
                                { editor
                                    | submitStatus = NotSubmitted { pressedSubmit = False }
                                    , pullRequestMessage = ""
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PressedToggleOnlyMissingTranslations ->
            case model.state of
                Editor editor ->
                    ( { model
                        | state =
                            Editor { editor | showOnlyMissingTranslations = not editor.showOnlyMissingTranslations }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TypedGithubUrl text ->
            case model.state of
                Start startModel ->
                    ( { model | state = Start { startModel | githubUrl = text } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


startDebounce : { a | changeCounter : Int } -> Cmd FrontendMsg
startDebounce editorModel =
    Process.sleep 2000
        |> Task.perform (\() -> DebounceFinished { changeCounter = editorModel.changeCounter + 1 })


saveChangesLocally : Dict TranslationId String -> Cmd msg
saveChangesLocally changes =
    local_storage_save_to_js
        { key = changesLocalStorageKey
        , value = Serialize.encodeToJson changesCodec changes |> Json.Encode.encode 0
        }


type ChangesVersion
    = ChangesV1 (Dict TranslationId String)


changesCodec : Serialize.Codec Cache.Error (Dict TranslationId String)
changesCodec =
    Serialize.customType
        (\a value ->
            case value of
                ChangesV1 data ->
                    a data
        )
        |> Serialize.variant1 ChangesV1 (assocListCodec translationIdCodec Serialize.string)
        |> Serialize.finishCustomType
        |> Serialize.map
            (\data ->
                case data of
                    ChangesV1 v1 ->
                        v1
            )
            ChangesV1


assocListCodec : Serialize.Codec e key -> Serialize.Codec e value -> Serialize.Codec e (Dict key value)
assocListCodec keyCodec valueCodec =
    Serialize.list (Serialize.tuple keyCodec valueCodec) |> Serialize.map Dict.fromList Dict.toList


translationIdCodec : Serialize.Codec Cache.Error TranslationId
translationIdCodec =
    Serialize.record TranslationId
        |> Serialize.field .filePath Serialize.string
        |> Serialize.field .functionName Serialize.string
        |> Serialize.field .path (Cache.nonemptyCodec Serialize.string)
        |> Serialize.finishRecord


startLoading :
    Github.OAuthToken
    -> Maybe Cache
    -> Owner
    -> String
    -> Maybe Github.Branch
    -> FrontendModel
    -> ( FrontendModel, Cmd frontendMsg )
startLoading oauthToken maybeCache owner repoName branch model =
    ( { model
        | state =
            Loading
                { filesRemaining = []
                , directoriesRemaining = Set.empty
                , oauthToken = oauthToken
                , fileContents = []
                , cache = maybeCache
                , owner = owner
                , repoName = repoName
                , branch = branch
                }
      }
    , Lamdera.sendToBackend (GetZipRequest { owner = owner, repoName = repoName, token = oauthToken, branch = branch })
    )


updateChanges : TranslationId -> String -> EditorModel -> ( EditorModel, Cmd FrontendMsg )
updateChanges translationId newText editorModel =
    ( { editorModel
        | changes =
            -- We don't directly update the translation text because we need to remove it if it matches the original translation.
            case Editor.getTranslation translationId editorModel.translations of
                Just (Ok ( _, { value } )) ->
                    case Editor.parseInput value newText of
                        Ok ok ->
                            if ok == value then
                                Dict.remove translationId editorModel.changes

                            else
                                Dict.insert translationId newText editorModel.changes

                        Err _ ->
                            Dict.insert translationId newText editorModel.changes

                _ ->
                    Dict.remove translationId editorModel.changes
        , changeCounter = editorModel.changeCounter + 1
      }
    , startDebounce editorModel
    )


createPullRequest :
    Github.OAuthToken
    -> Owner
    -> String
    -> Github.Branch
    -> Github.Branch
    -> Nonempty { path : String, content : String }
    -> String
    -> Task ( String, Http.Error ) { apiUrl : String, htmlUrl : String }
createPullRequest token owner repoName targetBranch newBranch changes message =
    retryGetBranch token 2 { repo = repoName, owner = owner } targetBranch
        |> Task.mapError (Tuple.pair "getBranch")
        |> Task.andThen
            (\commitSha ->
                Github.getCommit
                    { authToken = token
                    , repo = repoName
                    , owner = owner
                    , sha = commitSha
                    }
                    |> Task.mapError (Tuple.pair "getCommit")
                    |> Task.andThen
                        (\treeSha ->
                            Task.map2 (\_ a -> a)
                                (Github.createBranch
                                    { authToken = token
                                    , repo = repoName
                                    , owner = owner
                                    , branchName = newBranch
                                    , sha = commitSha
                                    }
                                    |> Task.mapError (Tuple.pair "createBranch")
                                )
                                (Github.createTree
                                    { authToken = token
                                    , owner = owner
                                    , repo = repoName
                                    , treeNodes = changes
                                    , baseTree = Just treeSha
                                    }
                                    |> Task.mapError (Tuple.pair "createTree")
                                    |> Task.andThen
                                        (\tree ->
                                            Github.createCommit
                                                { authToken = token
                                                , repo = repoName
                                                , owner = owner
                                                , message = "Edit translations"
                                                , tree = tree.treeSha
                                                , parents = [ commitSha ]
                                                }
                                                |> Task.mapError (Tuple.pair "createCommit")
                                        )
                                )
                        )
            )
        |> Task.andThen
            (\commitSha ->
                Github.updateBranch
                    { authToken = token
                    , owner = owner
                    , repo = repoName
                    , branchName = newBranch
                    , sha = commitSha
                    , force = False
                    }
                    |> Task.mapError (Tuple.pair "updateBranch")
            )
        |> Task.andThen
            (\_ ->
                Github.createPullRequest
                    { authToken = token
                    , sourceBranchOwner = owner
                    , destinationOwner = owner
                    , destinationRepo = repoName
                    , destinationBranch = targetBranch
                    , sourceBranch = newBranch
                    , title = "Edit translations"
                    , description = message
                    }
                    |> Task.mapError (Tuple.pair "createPullRequest")
            )


pullRequestMessage : String -> String -> String
pullRequestMessage name message =
    "Changes made by: "
        ++ (if String.trim name == "" then
                "*no name given*"

            else
                name
           )
        ++ "\n\n"
        ++ "Message: "
        ++ (if String.trim message == "" then
                "*no message*"

            else
                message
           )
        ++ "\n\n(This pull request was generated with "
        ++ Env.domain
        ++ ")"


retryGetBranch :
    Github.OAuthToken
    -> Int
    -> { a | repo : String, owner : Github.Owner }
    -> Github.Branch
    -> Task Http.Error (Github.ShaHash Github.CommitSha)
retryGetBranch token attemptsLeft fork branchName =
    Github.getBranch
        { authToken = token
        , repo = fork.repo
        , owner = fork.owner
        , branchName = branchName
        }
        |> Task.onError
            (\error ->
                case error of
                    Http.BadStatus 409 ->
                        if attemptsLeft > 0 then
                            Process.sleep 1000
                                |> Task.andThen (\() -> retryGetBranch token (attemptsLeft - 1) fork branchName)

                        else
                            Task.fail error

                    _ ->
                        Task.fail error
            )


authTokenLocalStorageKey : String
authTokenLocalStorageKey =
    "data"


ownerAndRepoNameToUrl : Owner -> String -> String
ownerAndRepoNameToUrl owner repoName =
    "https://github.com/" ++ Github.ownerToString owner ++ "/" ++ repoName


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        AuthenticateResponse result ->
            case model.state of
                Authenticate authenticate ->
                    case result of
                        Ok oauthToken ->
                            startLoading
                                oauthToken
                                authenticate.cache
                                authenticate.owner
                                authenticate.repoName
                                authenticate.branch
                                model

                        Err _ ->
                            ( { model
                                | state =
                                    Start
                                        { personalAccessToken = ""
                                        , pressedSubmit = False
                                        , loginFailed = True
                                        , cache = authenticate.cache
                                        , githubUrl = ownerAndRepoNameToUrl authenticate.owner authenticate.repoName
                                        , branch = authenticate.branch
                                        }
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        GetZipResponse result ->
            case model.state of
                Loading loadingModel ->
                    handleZipLoaded model.navKey result loadingModel
                        |> Tuple.mapFirst (\a -> { model | state = a })

                _ ->
                    ( model, Cmd.none )


handleZipLoaded : Browser.Navigation.Key -> Result Http.Error ( Github.Branch, Bytes ) -> LoadingModel -> ( State, Cmd FrontendMsg )
handleZipLoaded navigationKey result loadingModel =
    case result of
        Ok ( branch, zipBytes ) ->
            case Zip.fromBytes zipBytes of
                Just zip ->
                    let
                        elmModules =
                            List.filterMap
                                (\entry ->
                                    let
                                        path : String
                                        path =
                                            Zip.Entry.path entry
                                                |> String.split "/"
                                                |> List.drop 1
                                                |> String.join "/"
                                    in
                                    if
                                        String.endsWith ".elm" path
                                            && not (String.startsWith "tests" path)
                                            && not (String.startsWith "src/Evergreen/" path)
                                    then
                                        case Zip.Entry.toString entry of
                                            Ok contents ->
                                                Just ( path, contents )

                                            Err _ ->
                                                Nothing

                                    else
                                        Nothing
                                )
                                (Zip.entries zip)
                    in
                    parseFiles
                        navigationKey
                        { unparsedFiles = elmModules
                        , parsedFiles = []
                        , oauthToken = loadingModel.oauthToken
                        , loadedChanges = Dict.empty
                        , cache = loadingModel.cache
                        , owner = loadingModel.owner
                        , repoName = loadingModel.repoName
                        , branch = branch
                        }
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ cmd
                                    , local_storage_save_to_js
                                        { key = authTokenLocalStorageKey
                                        , value = Github.oauthTokenToString loadingModel.oauthToken
                                        }
                                    , local_storage_request_load_to_js { key = changesLocalStorageKey }
                                    ]
                            )

                Nothing ->
                    ( LoadFailed (Http.BadBody "Failed to read zip file")
                    , Cmd.none
                    )

        Err error ->
            case error of
                -- This means the user's login has expired
                Http.BadStatus 401 ->
                    ( Start
                        (initStart
                            { owner = Just loadingModel.owner
                            , repoName = Just loadingModel.repoName
                            , branch = loadingModel.branch
                            }
                        )
                    , Cmd.none
                    )

                _ ->
                    ( LoadFailed error, Cmd.none )


changesLocalStorageKey =
    "changes"


type alias RouteData =
    { code : Maybe OAuthCode
    , owner : Maybe Owner
    , repoName : Maybe String
    , branch : Maybe Github.Branch
    }


parseRoute : Url.Parser.Parser (RouteData -> a) a
parseRoute =
    Url.Parser.Query.map4
        (\code owner repoName branch ->
            { code = code
            , owner = Maybe.andThen validateOwner owner
            , repoName = Maybe.andThen validateRepoName repoName
            , branch = branch
            }
        )
        (Url.Parser.Query.string "code" |> Url.Parser.Query.map (Maybe.map Github.oauthCode))
        (Url.Parser.Query.string ownerParameterName)
        (Url.Parser.Query.string repoNameParameterName)
        (Url.Parser.Query.string branchParameterName |> Url.Parser.Query.map (Maybe.map Github.branch))
        |> Url.Parser.query


ownerParameterName =
    "owner"


repoNameParameterName =
    "repo-name"


branchParameterName =
    "branch"


view : FrontendModel -> Document FrontendMsg
view model =
    { title = "Translation editor"
    , body =
        [ Element.layout
            []
            (case model.state of
                Start startModel ->
                    startView startModel

                Loading loadingModel ->
                    let
                        loadedContents =
                            List.length loadingModel.fileContents

                        remainingFiles =
                            List.length loadingModel.filesRemaining
                    in
                    if loadedContents == 0 then
                        "Downloading repository..."
                            |> Element.text
                            |> Element.el [ Element.padding 16 ]

                    else
                        String.fromInt loadedContents
                            ++ "/"
                            ++ String.fromInt (remainingFiles + loadedContents)
                            ++ " modules loaded."
                            |> Element.text
                            |> Element.el [ Element.padding 16 ]

                Parsing { parsedFiles, unparsedFiles } ->
                    let
                        parsedCount =
                            List.length parsedFiles
                    in
                    String.fromInt parsedCount
                        ++ "/"
                        ++ String.fromInt (List.length unparsedFiles + parsedCount)
                        ++ " modules parsed."
                        |> Element.text
                        |> Element.el [ Element.padding 16 ]

                LoadFailed error ->
                    "Something went wrong when loading the repo: "
                        ++ httpToString error
                        |> Element.text
                        |> Element.el
                            [ Element.Font.color errorColor
                            , Element.padding 16
                            ]

                Editor editorModel ->
                    Editor.view
                        { windowWidth = model.windowWidth
                        , windowHeight = model.windowHeight
                        , changes = editorModel.changes
                        , translationData = editorModel.translations
                        }
                        editorModel

                ParsingFailed { path } ->
                    "Failed to parse "
                        ++ path
                        |> Element.text
                        |> Element.el
                            [ Element.Font.color errorColor
                            , Element.padding 16
                            ]

                Authenticate _ ->
                    Element.text "Authenticating..."
                        |> Element.el [ Element.padding 16 ]
            )
        ]
    }


groupTranslations : List TranslationDeclaration -> List TranslationGroup
groupTranslations translations_ =
    let
        translations : List ( Bool, { path : Nonempty String, filePath : String, functionName : String } )
        translations =
            List.concatMap
                (\translationDeclaration ->
                    Dict.toList translationDeclaration.translations
                        |> List.filterMap
                            (\( key, a ) ->
                                case a of
                                    Ok ok ->
                                        ( ok.isMarkdown /= Nothing
                                        , { path = key
                                          , filePath = translationDeclaration.filePath
                                          , functionName = translationDeclaration.functionName
                                          }
                                        )
                                            |> Just

                                    Err _ ->
                                        Nothing
                            )
                )
                translations_

        groupByPath : List ( ( Bool, TranslationId ), List ( Bool, TranslationId ) )
        groupByPath =
            List.gatherEqualsBy (\( _, a ) -> ( List.Nonempty.toList a.path, a.filePath )) translations
    in
    groupByPath
        |> List.map
            (\( ( isMarkdown, first ), rest ) ->
                { path = first.path
                , filePath = first.filePath
                , functionNames = List.Nonempty.map .functionName (Nonempty first (List.map Tuple.second rest))
                , isMarkdown =
                    case
                        ( List.any not (isMarkdown :: List.map Tuple.first rest)
                        , List.any identity (isMarkdown :: List.map Tuple.first rest)
                        )
                    of
                        ( True, True ) ->
                            IsPartiallyMarkdown

                        ( _, False ) ->
                            IsPlainText

                        ( False, _ ) ->
                            IsMarkdown
                }
            )


httpToString : Http.Error -> String
httpToString error =
    case error of
        Http.BadBody badBody ->
            badBody

        Http.BadUrl url ->
            "Invalid url " ++ url

        Http.Timeout ->
            "Connection timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Status code " ++ String.fromInt status


startView : StartModel -> Element FrontendMsg
startView model =
    Element.column
        [ Element.spacing 32, Element.centerX, Element.centerY ]
        [ Element.el [ Element.centerX, Element.Font.size 30 ] (Element.text "Elm Translation Editor")
        , if model.loginFailed then
            Element.el [ Element.Font.color errorColor ] (Element.text "Failed to login")

          else
            Element.none
        , Element.column
            [ Element.spacing 8, Element.width Element.fill ]
            [ if Env.isProduction then
                Element.link
                    (Element.width Element.fill :: Editor.buttonAttributes)
                    { url =
                        Github.oauthLink
                            { clientId = Env.clientId
                            , redirectUri = Just Env.domain
                            , scopes = [ Github.RepoScope ]
                            , state = Nothing
                            }
                    , label = Element.text "Login with OAuth"
                    }

              else
                Element.column
                    [ Element.spacing 8, Element.width Element.fill ]
                    [ Element.el
                        (Element.width Element.fill :: Element.Font.color (Element.rgb 0.5 0.5 0.5) :: Editor.buttonAttributes)
                        (Element.text "Login with OAuth")
                    , Element.text "Disabled when running locally"
                    ]
            ]
        , Element.column
            [ Element.spacing 24, Element.width Element.fill ]
            [ Element.column
                [ Element.spacing 8, Element.width Element.fill ]
                [ Element.Input.text
                    []
                    { onChange = TypedPersonalAccessToken
                    , text = model.personalAccessToken
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove [] (Element.text "Or use a personal access token")
                    }
                , case ( String.Nonempty.fromString model.personalAccessToken, model.pressedSubmit ) of
                    ( Nothing, True ) ->
                        Element.paragraph
                            [ Element.Font.color errorColor ]
                            [ Element.text "Enter your personal access token first" ]

                    _ ->
                        Element.none
                ]
            , Element.column
                [ Element.spacing 8, Element.width Element.fill ]
                [ Element.Input.text
                    []
                    { onChange = TypedGithubUrl
                    , text = model.githubUrl
                    , placeholder = Nothing
                    , label =
                        Element.Input.labelAbove
                            []
                            (Element.row
                                []
                                [ Element.text "Github repo url (i.e. https://github.com/owner/repo-name)"
                                ]
                            )
                    }
                , case ( validateGithubUrl model.githubUrl, model.pressedSubmit ) of
                    ( Nothing, True ) ->
                        Element.paragraph
                            [ Element.Font.color errorColor ]
                            [ Element.text "Choose which repo to translate" ]

                    _ ->
                        Element.none
                ]
            , Element.Input.button
                (Element.width Element.fill :: Editor.buttonAttributes)
                { onPress = Just PressedSubmitPersonalAccessToken, label = Element.text "Submit token" }
            ]
        ]


errorColor =
    Element.rgb 0.95 0.1 0.1
