module MergeTest exposing (..)

import AssocList as Dict
import Editor exposing (Error(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Expect
import Github
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Set
import Test exposing (describe, test)
import TranslationParser exposing (Content(..))
import TranslationTestCode
import Types exposing (EditorModel, SubmitStatus(..))


tests =
    describe "Merge translations together"
        [ test "Parse user input" <|
            \_ ->
                Editor.parseInput (Nonempty (TextContent "Test") []) "TestTest"
                    |> Expect.equal (Ok (Nonempty (TextContent "TestTest") []))
        , test "Parse user input with placeholder" <|
            \_ ->
                Editor.parseInput
                    (Nonempty
                        (TextContent "Test")
                        [ Placeholder (FunctionOrValue [] "blah") ]
                    )
                    "Test{blah}Test"
                    |> Expect.equal
                        (Ok
                            (Nonempty (TextContent "Test")
                                [ Placeholder (FunctionOrValue [] "blah")
                                , TextContent "Test"
                                ]
                            )
                        )
        , test "Parse user input with extra curly braces" <|
            \_ ->
                Editor.parseInput
                    (Nonempty
                        (TextContent "Test")
                        [ Placeholder (FunctionOrValue [] "blah") ]
                    )
                    "Tes{}t{blah}Tes{t"
                    |> Expect.equal
                        (Ok
                            (Nonempty (TextContent "Tes{}t")
                                [ Placeholder (FunctionOrValue [] "blah")
                                , TextContent "Tes{t"
                                ]
                            )
                        )
        , test "Parse user input fails if placeholder is missing" <|
            \_ ->
                Editor.parseInput
                    (Nonempty
                        (TextContent "Test")
                        [ Placeholder (FunctionOrValue [] "blah") ]
                    )
                    "Tes{}t{bla}Tes{t"
                    |> Expect.equal
                        (Err (Nonempty (PlaceholderMissing "blah") []))
        , test "Parse user input fails if a placeholder appears multiple times" <|
            \_ ->
                Editor.parseInput
                    (Nonempty
                        (TextContent "Test")
                        [ Placeholder (FunctionOrValue [] "blah") ]
                    )
                    "Tes{}t{blah}{blah}Tes{t"
                    |> Expect.equal
                        (Err (Nonempty (PlaceholderRepeated "blah") []))
        , test "No changes causes applyChanges to fail" <|
            \_ ->
                case TranslationParser.parse "A.elm" exampleCode of
                    Ok translationDeclaration ->
                        let
                            editorModel : EditorModel
                            editorModel =
                                { files = Dict.fromList [ ( "A.elm", { original = exampleCode } ) ]
                                , translations = translationDeclaration
                                , oauthToken = Github.oauthToken ""
                                , changes = Dict.fromList []
                                , submitStatus = NotSubmitted { pressedSubmit = False }
                                , pullRequestMessage = ""
                                , hiddenLanguages = Set.empty
                                , showOnlyMissingTranslations = False
                                , changeCounter = 0
                                , allLanguages = Set.fromList [ "en", "sv" ]
                                , groups = []
                                , name = ""
                                , branch = Github.branch "main"
                                }
                        in
                        Editor.applyChanges editorModel
                            |> Expect.equal (Err ())

                    Err _ ->
                        Expect.fail "Failed to parse"
        , test "Apply change" <|
            \_ ->
                let
                    expected =
                        Ok (Nonempty { content = "module A exposing (..)\n\nenglishTexts =\n    { form =\n        { emailAddressNotValid = \"This email is not valid\"\n        , emailAddressText = \"E-mail\"\n        , termsOfService = \\urls -> \"By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](\" ++ urls.termsOfService ++ \").\"\n        }\n    , success =\n        { checkYourEmail = \"Check your e-mail\"\n        , emailSent = \\email -> \"We have sent an e-mail to **\" ++ Email.toString email ++ \"**.\n\nClick the button in the email to log in.\"\n        }\n    , emailSentInstructions = \"Testtest\"\n    }\n    \nswedishTexts =\n    { form =\n        { emailAddressNotValid = \"(Swedish) This email is not valid\"\n        , emailAddressText = \"(Swedish) E-mail\"\n        , termsOfService = \\urls -> \"(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](\" ++ urls.termsOfService ++ \").\"\n        }\n    , success =\n        { checkYourEmail = \"(Swedish) Check your e-mail\"\n        , emailSent = \\email -> \"(Swedish) We have sent an e-mail to **\" ++ Email.toString email ++ \"**.\n\nClick the button in the email to log in.\"\n        }\n    , emailSentInstructions = \"(Swedish) If you didn’t receive an e-mail from us, check your **Spam folder**, and make sure that the e-mail address is correctly entered.\"\n    }\n", path = "A.elm" } [])
                in
                case TranslationParser.parse "A.elm" exampleCode of
                    Ok translationDeclaration ->
                        let
                            editorModel : EditorModel
                            editorModel =
                                { files = Dict.fromList [ ( "A.elm", { original = exampleCode } ) ]
                                , translations = translationDeclaration
                                , oauthToken = Github.oauthToken ""
                                , changes =
                                    Dict.fromList
                                        [ ( { filePath = "A.elm"
                                            , functionName = "englishTexts"
                                            , path = Nonempty "emailSentInstructions" []
                                            }
                                          , "Testtest"
                                          )
                                        ]
                                , submitStatus = NotSubmitted { pressedSubmit = False }
                                , pullRequestMessage = ""
                                , hiddenLanguages = Set.empty
                                , changeCounter = 0
                                , showOnlyMissingTranslations = False
                                , allLanguages = Set.fromList [ "en", "sv" ]
                                , groups = []
                                , name = ""
                                , branch = Github.branch "main"
                                }
                        in
                        Editor.applyChanges editorModel |> Expect.equal expected

                    Err _ ->
                        Expect.fail "Failed to parse"
        , describe "Parse tests"
            [ test "test" <|
                \_ ->
                    case TranslationParser.parse "A.elm" TranslationTestCode.code of
                        Ok translations ->
                            List.find (.functionName >> (==) "swedishTexts") translations
                                |> Maybe.map
                                    (\swedishTranslation ->
                                        Dict.get
                                            (Nonempty "success" [ "form3", "emailAddressText" ])
                                            swedishTranslation.translations
                                    )
                                |> Expect.equal
                                    (Just
                                        (Just
                                            (Ok
                                                { value = Nonempty (TextContent "🚧") []
                                                , range =
                                                    { start = { column = 33, row = 72 }
                                                    , end = { column = 36, row = 72 }
                                                    }
                                                , isMarkdown = Nothing
                                                }
                                            )
                                        )
                                    )

                        Err _ ->
                            Expect.fail "Failed to parse"
            , test "markdown test" <|
                \_ ->
                    case TranslationParser.parse "A.elm" TranslationTestCode.code of
                        Ok translations ->
                            List.find
                                (\translation ->
                                    translation.functionName == "englishTexts"
                                )
                                translations
                                |> Maybe.map
                                    (\englishTranslations ->
                                        Dict.get
                                            (Nonempty "form" [ "termsOfService" ])
                                            englishTranslations.translations
                                    )
                                |> Expect.equal
                                    (Just
                                        (Just
                                            (Ok
                                                { range = { end = { column = 193, row = 8 }, start = { column = 36, row = 8 } }
                                                , value =
                                                    Nonempty
                                                        (TextContent "By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](")
                                                        [ Placeholder (RecordAccess (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "urls")) (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } "termsOfService"))
                                                        , TextContent ")."
                                                        ]
                                                , isMarkdown = Just (FunctionOrValue [ "Markdown" ] "fromString")
                                                }
                                            )
                                        )
                                    )

                        Err _ ->
                            Expect.fail "Failed to parse"
            ]
        ]


exampleCode =
    """module A exposing (..)

englishTexts =
    { form =
        { emailAddressNotValid = "This email is not valid"
        , emailAddressText = "E-mail"
        , termsOfService = \\urls -> "By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
        }
    , success =
        { checkYourEmail = "Check your e-mail"
        , emailSent = \\email -> "We have sent an e-mail to **" ++ Email.toString email ++ "**.

Click the button in the email to log in."
        }
    , emailSentInstructions = "If you didn’t receive an e-mail from us, check your **Spam folder**, and make sure that the e-mail address is correctly entered."
    }
    
swedishTexts =
    { form =
        { emailAddressNotValid = "(Swedish) This email is not valid"
        , emailAddressText = "(Swedish) E-mail"
        , termsOfService = \\urls -> "(Swedish) By clicking on **Log in** you agree to the use of cookies. Read more in our [complete cookie policy](" ++ urls.termsOfService ++ ")."
        }
    , success =
        { checkYourEmail = "(Swedish) Check your e-mail"
        , emailSent = \\email -> "(Swedish) We have sent an e-mail to **" ++ Email.toString email ++ "**.

Click the button in the email to log in."
        }
    , emailSentInstructions = "(Swedish) If you didn’t receive an e-mail from us, check your **Spam folder**, and make sure that the e-mail address is correctly entered."
    }
"""
