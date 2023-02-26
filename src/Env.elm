module Env exposing (..)

import Github exposing (ClientId, ClientSecret)


domain : String
domain =
    "http://localhost:8000"


clientSecret_ : String
clientSecret_ =
    ""


clientSecret : ClientSecret
clientSecret =
    Github.clientSecret clientSecret_


clientId_ : String
clientId_ =
    ""


clientId : ClientId
clientId =
    Github.clientId clientId_


isProduction_ : String
isProduction_ =
    "false"


isProduction : Bool
isProduction =
    String.toLower isProduction_ == "true"
