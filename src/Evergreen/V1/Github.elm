module Evergreen.V1.Github exposing (..)


type Branch
    = Branch String


type Owner
    = Owner String


type OAuthToken
    = OAuthToken String


type OAuthCode
    = OAuthCode String


type alias AccessTokenResponse =
    { accessToken : OAuthToken
    , scope : String
    , tokenType : String
    }
