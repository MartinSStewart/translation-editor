module Editor exposing (..)

import Array
import AssocList as Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Lazy
import Elm.Pretty
import Elm.Syntax.Range exposing (Range)
import Env
import Github
import Html
import Html.Attributes
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Pretty
import Set exposing (Set)
import TranslationParser exposing (Content(..), TranslationDeclaration, TranslationValue)
import Types exposing (EditorModel, FrontendMsg(..), SubmitStatus(..), TranslationGroup, TranslationId)


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , changes : Dict TranslationId String
    , translationData : List TranslationDeclaration
    }


type Error
    = PlaceholderMissing String
    | PlaceholderRepeated String


parseInput : Nonempty Content -> String -> Result (Nonempty Error) (Nonempty Content)
parseInput originalContent userText =
    String.foldl
        (\char state ->
            let
                text =
                    String.fromChar char
            in
            case ( char, state.foundPlaceholder ) of
                ( '{', _ ) ->
                    { state
                        | currentText = "{"
                        , content = TextContent state.currentText :: state.content
                        , foundPlaceholder = True
                    }

                ( '}', True ) ->
                    let
                        key =
                            String.dropLeft 1 state.currentText |> String.trim
                    in
                    case Dict.get key state.placeholders of
                        Just ( counter, expression ) ->
                            { state
                                | currentText = ""
                                , content = Placeholder expression :: state.content
                                , foundPlaceholder = False
                                , placeholders = Dict.insert key ( counter + 1, expression ) state.placeholders
                            }

                        Nothing ->
                            { state
                                | currentText = state.currentText ++ "}"
                                , foundPlaceholder = False
                            }

                ( _, _ ) ->
                    { state | currentText = state.currentText ++ text }
        )
        { currentText = ""
        , content = []
        , foundPlaceholder = False
        , placeholders =
            TranslationParser.getPlaceholders originalContent
                |> TranslationParser.namesToPlaceholders
                |> Dict.map (\_ v -> ( 0, v ))
        }
        userText
        |> (\{ currentText, content, placeholders } ->
                let
                    errors =
                        Dict.toList placeholders
                            |> List.filterMap
                                (\( key, ( count, _ ) ) ->
                                    if count < 1 then
                                        Just (PlaceholderMissing key)

                                    else if count > 1 then
                                        Just (PlaceholderRepeated key)

                                    else
                                        Nothing
                                )
                in
                case List.Nonempty.fromList errors of
                    Just nonemptyErrors ->
                        Err nonemptyErrors

                    Nothing ->
                        TextContent currentText
                            :: content
                            |> List.reverse
                            |> List.Nonempty.fromList
                            |> Maybe.withDefault (List.Nonempty.fromElement (TextContent ""))
                            |> mergeTextContent
                            |> Ok
           )


mergeTextContent : Nonempty Content -> Nonempty Content
mergeTextContent nonempty =
    List.foldl
        (\next state ->
            case ( List.Nonempty.head state, next ) of
                ( TextContent text, TextContent nextText ) ->
                    List.Nonempty.replaceHead (TextContent (text ++ nextText)) state

                _ ->
                    List.Nonempty.cons next state
        )
        (List.Nonempty.fromElement <| List.Nonempty.head nonempty)
        (List.Nonempty.tail nonempty)
        |> List.Nonempty.reverse


submitConfirmView : EditorModel -> Element FrontendMsg
submitConfirmView editorModel =
    Element.column
        [ Element.width <| Element.maximum 800 Element.fill
        , Element.centerX
        , Element.spacing 24
        , Element.padding 16
        ]
        [ Element.el [ Element.centerX, Element.Font.size 30 ] (Element.text "Confirm changes")
        , Element.paragraph
            [ Element.width Element.fill ]
            [ Element.el [ Element.Font.bold ] (Element.text "Important!")
            , Element.text " Once submitted, you won't be able to edit your changes and this editor will be reset to fresh state. A programmer will review your changes and then merge them into the app."
            ]
        , Element.Input.multiline
            [ Element.width Element.fill, Element.height <| Element.minimum 200 Element.shrink ]
            { onChange = TypedPullRequestMessage
            , text = editorModel.pullRequestMessage
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove
                    []
                    (Element.paragraph
                        []
                        [ Element.text "Summarize your changes (optional)" ]
                    )
            , spellcheck = True
            }
        , changesSummaryView editorModel.changes
        , Element.row
            [ Element.width Element.fill, Element.spacing 8 ]
            [ case editorModel.submitStatus of
                SubmitSuccessful _ ->
                    Element.Input.button
                        (buttonAttributes ++ [ Element.Background.color (Element.rgb 0.8 0.9 1) ])
                        { onPress = Just PressedCloseSubmitSuccessful, label = Element.text "Close" }

                _ ->
                    Element.Input.button
                        buttonAttributes
                        { onPress = Just PressedCancelSubmitChanges, label = Element.text "Cancel" }
            , Element.row
                [ Element.alignRight, Element.spacing 8 ]
                [ submitStatusMessage False editorModel.submitStatus
                , Element.Input.button
                    buttonAttributes
                    { onPress = Just PressedConfirmSubmitChanges, label = Element.text "Confirm changes" }
                ]
            ]
        ]


changesSummaryView : Dict TranslationId String -> Element msg
changesSummaryView changes =
    Dict.keys changes
        |> List.gatherEqualsBy (\key -> TranslationParser.getLanguageShortName key.functionName)
        |> List.map
            (\( key, rest ) ->
                let
                    changeCount =
                        List.length rest + 1

                    language : Maybe String
                    language =
                        TranslationParser.getLanguageLongName key.functionName
                in
                String.fromInt changeCount
                    ++ (if changeCount == 1 then
                            " change made to "

                        else
                            " changes made to "
                       )
                    ++ Maybe.withDefault key.functionName language
                    ++ " translations"
                    |> Element.text
            )
        |> Element.column []


view : Model -> EditorModel -> Element FrontendMsg
view model editorModel =
    if List.isEmpty editorModel.translations then
        let
            url =
                Github.ownerToString Env.owner ++ "/" ++ Env.repo
        in
        Element.column
            [ Element.centerX
            , Element.centerY
            , Element.width <| Element.maximum 800 Element.fill
            , Element.spacing 16
            ]
            [ Element.paragraph
                []
                [ Element.text "No translations found in "
                , Element.newTabLink
                    [ Element.Font.color linkColor ]
                    { url = "https://github.com/" ++ url, label = Element.text url }
                ]
            , Element.paragraph [] [ Element.text "Translations must match this format:" ]
            , """myTranslationsEnglish =
    { showName = \\name -> "Hi my name is " ++ name ++ "!"
    , login = "Login"
    , yes = "Yes"
    , email = \\email -> "Your email is " ++ Email.toString email ++ "."
    }

-- Function must contain a language in the name
myTranslationsSwedish =
    { showName = \\name -> "Hej jag heter " ++ name ++ "!"
    , login = "Logga in"
    , yes = "Ja"
    , email = \\email -> "Din e-post är " ++ Email.toString email ++ "."
    }
"""
                |> Html.text
                |> List.singleton
                |> Html.div
                    [ Html.Attributes.style "white-space" "pre"
                    , Html.Attributes.style "line-height" "24px"
                    ]
                |> Element.html
                |> Element.el [ Element.Font.family [ Element.Font.monospace ] ]
            ]

    else
        case editorModel.submitStatus of
            NotSubmitted _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.Background.color (Element.rgb 0.5 0.5 0.5)
                    ]
                    [ headerView
                        editorModel.allLanguages
                        editorModel.hiddenLanguages
                        editorModel.submitStatus
                        (Dict.isEmpty editorModel.changes)
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height <| Element.px (model.windowHeight - headerHeight)
                        ]
                        (Element.column
                            [ Element.width Element.fill
                            , Element.spacing 8
                            , Element.padding 8
                            , Element.scrollbarY
                            ]
                            (List.map
                                (translationView editorModel.hiddenLanguages model.translationData model.changes)
                                editorModel.groups
                            )
                        )
                    ]

            _ ->
                submitConfirmView editorModel


headerHeight : number
headerHeight =
    50


hideLanguageView : Set String -> Set String -> Element FrontendMsg
hideLanguageView allLanguages hiddenLanguages =
    let
        attributes =
            [ Element.Font.color white
            , Element.height Element.fill
            , Element.padding 8
            , Element.Border.rounded 4
            ]
    in
    Set.toList allLanguages
        |> List.sort
        |> List.map
            (\language ->
                if Set.member language hiddenLanguages then
                    Element.Input.button
                        (Element.Background.color lightPurple :: attributes)
                        { onPress = Just (PressedShowLanguage language)
                        , label = Element.text language
                        }

                else
                    Element.Input.button
                        (Element.Background.color purple :: attributes)
                        { onPress = Just (PressedHideLanguage language)
                        , label = Element.text language
                        }
            )
        |> Element.row [ Element.spacing 4 ]


buttonAttributes : List (Element.Attribute msg)
buttonAttributes =
    [ Element.Border.width 1
    , Element.Border.color (Element.rgb 0 0 0)
    , Element.paddingXY 12 7
    , Element.Border.rounded 4
    , Element.Background.color (Element.rgb 0.9 0.9 0.9)
    ]


headerView : Set String -> Set String -> SubmitStatus -> Bool -> Element FrontendMsg
headerView allLanguages hiddenLanguages submitStatus noChanges =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color white
        , Element.paddingXY 8 0
        , Element.height (Element.px headerHeight)
        , Element.spacing 48
        ]
        [ Element.row
            [ Element.spacing 8 ]
            [ Element.text "Filter by language", hideLanguageView allLanguages hiddenLanguages ]
        , Element.row
            [ Element.spacing 8 ]
            [ Element.Input.button
                buttonAttributes
                { onPress = Just PressedSubmitChanges
                , label = Element.text "Submit changes"
                }
            , submitStatusMessage noChanges submitStatus
            ]
        ]


submitStatusMessage : Bool -> SubmitStatus -> Element msg
submitStatusMessage noChanges submitStatus =
    case submitStatus of
        NotSubmitted { pressedSubmit } ->
            if noChanges && pressedSubmit then
                errorMessage "There are no changes to submit"

            else
                Element.el
                    [ Element.Font.size 16
                    , Element.Font.color (Element.rgb 0.3 0.3 0.3)
                    ]
                    (Element.text "Changes made are auto-saved locally")

        SubmitConfirm _ ->
            Element.none

        Submitting ->
            Element.text "Submitting..."

        SubmitFailed { error, pressedSubmit } ->
            case ( noChanges, pressedSubmit ) of
                ( True, True ) ->
                    errorMessage "There are no changes to submit"

                ( _, False ) ->
                    errorMessage ("Submit failed at " ++ Tuple.first error)

                ( False, True ) ->
                    Element.none

        SubmitSuccessful { htmlUrl } ->
            Element.newTabLink
                [ Element.Font.color linkColor, Element.Font.underline ]
                { url = htmlUrl, label = Element.text "Link to pull request" }


linkColor =
    Element.rgb 0.1 0.1 1


errorMessage : String -> Element msg
errorMessage text =
    Element.el [ Element.Font.color errorColor ] (Element.text text)


translationView :
    Set String
    -> List TranslationDeclaration
    -> Dict TranslationId String
    -> TranslationGroup
    -> Element FrontendMsg
translationView hiddenLanguages translationData changes translationGroup =
    let
        hasNoChanges =
            List.Nonempty.toList translationGroup.ids
                |> List.filterMap
                    (\id ->
                        Dict.get
                            { path = translationGroup.path
                            , functionName = id.functionName
                            , filePath = id.filePath
                            }
                            changes
                    )
                |> List.isEmpty
    in
    Element.column
        [ Element.spacing 8
        , Element.Border.rounded 4
        , Element.Background.color (Element.rgb 1 1 1)
        , Element.padding 8
        ]
        [ Element.row
            [ Element.width Element.fill, Element.height (Element.px 30) ]
            [ Element.text (List.Nonempty.toList translationGroup.path |> String.join ".")
            , if hasNoChanges then
                Element.none

              else
                Element.Input.button
                    (Element.alignRight :: buttonAttributes ++ [ Element.padding 4 ])
                    { onPress = Just (PressedResetTranslationGroup { path = translationGroup.path })
                    , label = Element.text "Reset"
                    }
            ]
        , Element.column
            [ Element.spacing 8, Element.width Element.fill ]
            (List.Nonempty.toList translationGroup.ids
                |> List.map
                    (\id ->
                        case TranslationParser.getLanguageShortName id.functionName of
                            Just language ->
                                if Set.member language hiddenLanguages then
                                    Element.none

                                else
                                    case
                                        Dict.get
                                            { path = translationGroup.path
                                            , functionName = id.functionName
                                            , filePath = id.filePath
                                            }
                                            changes
                                    of
                                        Just change ->
                                            Element.Lazy.lazy5
                                                translationInputWithChange
                                                translationData
                                                change
                                                translationGroup.path
                                                id.functionName
                                                id.filePath

                                        Nothing ->
                                            Element.Lazy.lazy5
                                                translationInput
                                                translationData
                                                Nothing
                                                translationGroup.path
                                                id.functionName
                                                id.filePath

                            Nothing ->
                                Element.none
                    )
            )
        ]


getTranslation :
    TranslationId
    -> List TranslationDeclaration
    -> Maybe (Result () ( TranslationDeclaration, { value : Nonempty Content, range : Range } ))
getTranslation translationId translationDeclarations =
    List.filterMap
        (\translation ->
            if
                (translationId.filePath == translation.filePath)
                    && (translationId.functionName == translation.functionName)
            then
                Dict.get translationId.path translation.translations
                    |> Maybe.map (Result.map (Tuple.pair translation))

            else
                Nothing
        )
        translationDeclarations
        |> List.head


purple =
    Element.rgb255 102 68 153


lightPurple =
    Element.rgb255 152 128 193


translationInputWithChange :
    List TranslationDeclaration
    -> String
    -> Nonempty String
    -> String
    -> String
    -> Element FrontendMsg
translationInputWithChange a b c d e =
    translationInput a (Just b) c d e


translationInput :
    List TranslationDeclaration
    -> Maybe String
    -> Nonempty String
    -> String
    -> String
    -> Element FrontendMsg
translationInput translationData change translationGroup functionName filePath =
    let
        translationId : TranslationId
        translationId =
            { path = translationGroup, functionName = functionName, filePath = filePath }
    in
    case getTranslation translationId translationData of
        Just (Ok ( _, translation )) ->
            Element.column
                [ Element.width Element.fill, Element.spacing 6, Element.Font.size 16 ]
                [ Element.Input.multiline
                    [ Element.Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 4, bottomRight = 4 }
                    , Element.width <| Element.px 740
                    , Element.spacingXY 0 8
                    ]
                    { onChange = TypedTranslation translationId
                    , text = Maybe.withDefault (TranslationParser.contentToString translation.value) change
                    , placeholder = Nothing
                    , label =
                        Element.Input.labelLeft
                            [ Element.Background.color purple
                            , Element.Font.color white
                            , Element.width <| Element.px 40
                            , Element.height Element.fill
                            , Element.padding 8
                            , Element.Border.roundEach
                                { topLeft = 4, bottomLeft = 4, topRight = 0, bottomRight = 0 }
                            ]
                            (Element.el
                                [ Element.centerX, Element.centerY ]
                                (Element.text
                                    (TranslationParser.getLanguageShortName
                                        translationId.functionName
                                        |> Maybe.withDefault ""
                                    )
                                )
                            )
                    , spellcheck = True
                    }
                , case change of
                    Just userInput ->
                        case parseInput translation.value userInput of
                            Ok _ ->
                                Element.none

                            Err errors ->
                                Element.paragraph
                                    [ Element.paddingEach { left = 4, right = 0, top = 0, bottom = 0 }
                                    , Element.Font.color errorColor
                                    ]
                                    [ Element.text
                                        (case List.Nonempty.head errors of
                                            PlaceholderMissing name ->
                                                "{" ++ name ++ "} is missing"

                                            PlaceholderRepeated name ->
                                                "{" ++ name ++ "} can only appear once"
                                        )
                                    ]

                    Nothing ->
                        Element.none
                ]

        Just (Err ()) ->
            Element.text "Couldn't use translation, invalid format."

        Nothing ->
            "TranslationId not found: "
                ++ translationId.filePath
                ++ " "
                ++ translationId.functionName
                ++ " "
                ++ String.join "." (List.Nonempty.toList translationId.path)
                |> Element.text


applyChanges : EditorModel -> Result () (Nonempty { path : String, content : String })
applyChanges model =
    Dict.toList model.changes
        |> List.filterMap
            (\( translationId, text ) ->
                case getTranslation translationId model.translations of
                    Just (Ok ( translationDeclaration, translation )) ->
                        case parseInput translation.value text of
                            Ok parsedText ->
                                let
                                    code =
                                        TranslationParser.writeContents parsedText
                                            |> Elm.Pretty.prettyExpression
                                            |> Pretty.pretty 100
                                in
                                Just ( translationDeclaration.filePath, translation.range, code )

                            Err _ ->
                                Nothing

                    _ ->
                        Nothing
            )
        |> List.gatherEqualsBy (\( filePath, _, _ ) -> filePath)
        |> List.filterMap
            (\( ( filePath, range, change ), rest ) ->
                case Dict.get filePath model.files of
                    Just file ->
                        case
                            tryToApplyFix
                                (( range, change ) :: List.map (\( _, a, b ) -> ( a, b )) rest)
                                file.original
                        of
                            Ok ok ->
                                if ok == file.original then
                                    Nothing

                                else
                                    Just { path = filePath, content = ok }

                            Err () ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        |> List.Nonempty.fromList
        |> Result.fromMaybe ()


white : Element.Color
white =
    Element.rgb 1 1 1


errorColor : Element.Color
errorColor =
    Element.rgb 0.9 0.1 0.1


tryToApplyFix : List ( Range, String ) -> String -> Result () String
tryToApplyFix fixes sourceCode =
    let
        resultAfterFix : String
        resultAfterFix =
            fixes
                |> List.sortBy (rangePosition >> negate)
                |> List.foldl applyFix (String.lines sourceCode)
                |> String.join "\n"
    in
    if sourceCode == resultAfterFix then
        Err ()

    else
        Ok resultAfterFix


rangePosition : ( Range, String ) -> Int
rangePosition ( range, _ ) =
    positionAsInt range.start


positionAsInt : { row : Int, column : Int } -> Int
positionAsInt { row, column } =
    -- This is a quick and simple heuristic to be able to sort ranges.
    -- It is entirely based on the assumption that no line is longer than
    -- 1.000.000 characters long. Then, as long as ranges don't overlap,
    -- this should work fine.
    row * 1000000 + column


applyFix : ( Range, String ) -> List String -> List String
applyFix ( range, replacement ) lines =
    applyReplace range replacement lines


applyReplace : Range -> String -> List String -> List String
applyReplace range replacement lines =
    let
        linesBefore : List String
        linesBefore =
            lines
                |> List.take (range.start.row - 1)

        linesAfter : List String
        linesAfter =
            lines
                |> List.drop range.end.row

        startLine : String
        startLine =
            getRowAtLine lines (range.start.row - 1)
                |> String.slice 0 (range.start.column - 1)

        endLine : String
        endLine =
            getRowAtLine lines (range.end.row - 1)
                |> String.dropLeft (range.end.column - 1)
    in
    List.concat
        [ linesBefore
        , startLine ++ replacement ++ endLine |> String.lines
        , linesAfter
        ]


getRowAtLine : List String -> Int -> String
getRowAtLine lines rowIndex =
    case lines |> Array.fromList |> Array.get rowIndex of
        Just line ->
            if String.trim line /= "" then
                line

            else
                ""

        Nothing ->
            ""
