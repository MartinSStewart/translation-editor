module Evergreen.V8.Github exposing (..)


type OAuthToken
    = OAuthToken String


type OAuthCode
    = OAuthCode String


type alias AccessTokenResponse =
    { accessToken : OAuthToken
    , scope : String
    , tokenType : String
    }
