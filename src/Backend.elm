module Backend exposing (..)

import Env
import Github
import Lamdera exposing (ClientId, SessionId)
import Task
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( {}
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        GotAccessToken clientId result ->
            ( model
            , Lamdera.sendToFrontend
                clientId
                (AuthenticateResponse (Result.map .accessToken result))
            )

        LoadedZipBackend clientId result ->
            ( model, Lamdera.sendToFrontend clientId (GetZipResponse result) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        AuthenticateRequest token ->
            ( model
            , Github.getAccessToken
                { clientId = Env.clientId
                , clientSecret = Env.clientSecret
                , oauthCode = token
                , state = Nothing
                }
                |> Task.attempt (GotAccessToken clientId)
            )

        GetZipRequest oauthToken maybeBranch ->
            let
                getBranch =
                    case maybeBranch of
                        Just branch ->
                            Task.succeed branch

                        Nothing ->
                            Github.getRepository { authToken = oauthToken, owner = Env.owner, repo = Env.repo }
                                |> Task.map .defaultBranch
            in
            ( model
            , getBranch
                |> Task.andThen
                    (\branch ->
                        Github.getBranchZip
                            { authToken = Nothing, owner = Env.owner, repo = Env.repo, branchName = Just branch }
                            |> Task.onError
                                (\_ ->
                                    -- Sometimes auth will cause the request to fail if it wasn't needed so we try again without auth here.
                                    Github.getBranchZip
                                        { authToken = Just oauthToken, owner = Env.owner, repo = Env.repo, branchName = Just branch }
                                )
                            |> Task.map (Tuple.pair branch)
                    )
                |> Task.attempt (LoadedZipBackend clientId)
            )
