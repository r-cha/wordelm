module Main exposing (..)

-- Wordle clone in Elm because ✨ why not ✨

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import String
import WordList exposing (getTodaysWord, getWordList)



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL

type GameState
    = Playing
    | Won
    | Lost

type alias Model =
    { current : List Char
    , guesses : List (List ( Char, Mark ))
    , answer : String
    , words : List String
    , now : Int
    , error : String
    , gameState : GameState
    , message : String
    }

emptyModel : Int -> Model
emptyModel now =
    { current = []
    , guesses = []
    , answer = "OPALS"
    , words = []
    , now = now
    , error = ""
    , gameState = Playing
    , message = ""
    }


type alias Mark =
    -- -1=unknown, 0=absent, 1=present, 2=correct
    Int


toString : Mark -> String
toString mark =
    case mark of
        0 ->
            "absent"

        1 ->
            "present"

        2 ->
            "correct"

        _ ->
            "unknown"


init : Int -> ( Model, Cmd Msg )
init now =
    ( emptyModel now
    , getWordList
        |> Cmd.map WordsLoaded
    )



-- UPDATE


type Msg
    = NoOp
    | Character Char
    | Control String
    | WordsLoaded (Result Http.Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Character char ->
            -- Process a letter being typed.
            if model.gameState /= Playing then
                -- If the game is not in the 'Playing' state, ignore the input
                ( model, Cmd.none )

            else if (List.length model.current < 5) && Char.isAlpha char then
                -- Only if there is a room and it's a letter
                ( { model | current = char :: model.current, error = "" }
                , Cmd.none
                )

            else
                -- Truly ignore errant input - no message
                ( { model | error = "" }, Cmd.none )

        Control "Enter" ->
            if model.gameState /= Playing then
                -- If the game is not in the 'Playing' state, ignore the input
                ( model, Cmd.none )

            else if List.length model.current /= 5 then
                -- This should only do anything if there are 5 letters available
                ( { model | error = "Not enough letters" }, Cmd.none )

            else
                let
                    guess =
                        List.reverse model.current
                in
                if not (List.member (String.fromList guess) model.words) then
                    -- AND the guess is a word
                    ( { model | error = "Not in word list" }, Cmd.none )

                else
                    let
                        newGuesses = checkGuess guess model.answer :: model.guesses
                        newGameState =
                            if String.fromList guess == model.answer then
                                Won
                            else if List.length newGuesses == 6 then
                                Lost
                            else
                                Playing
                        newMessage =
                            case newGameState of
                                Won ->
                                    "Congratulations! You guessed the word!"

                                Lost ->
                                    "Sorry, you've used all your guesses. Try again tomorrow!"

                                _ ->
                                    ""
                    in
                    ( { model
                        | current = []
                        , guesses = newGuesses
                        , error = ""
                        , gameState = newGameState
                        , message = newMessage
                      }
                    , Cmd.none
                    )

        Control "Backspace" ->
            -- Remove the last letter
            let
                old =
                    case model.current of
                        _ :: tail ->
                            tail

                        [] ->
                            []
            in
            ( { model | current = old, error = "" }, Cmd.none )

        WordsLoaded (Ok words) ->
            let
                answer =
                    getTodaysWord model.now words
            in
            ( { model | words = words, answer = answer, error = "" }, Cmd.none )

        WordsLoaded (Err _) ->
            ( { model | error = "Word list could not be loaded!" }, Cmd.none )

        _ ->
            ( { model | error = "" }, Cmd.none )


checkChars : String -> ( Char, Char ) -> ( Char, Mark )
checkChars fullAnswer ( guess, answer ) =
    if guess == answer then
        ( guess, 2 )

    else if String.contains (String.fromChar guess) fullAnswer then
        ( guess, 1 )

    else
        ( guess, 0 )


checkGuess : List Char -> String -> List ( Char, Mark )
checkGuess guess answer =
    let
        answerList = String.toList answer
        initialMarks = List.map2 (\g a -> if g == a then 2 else -1) guess answerList
        answerCharCounts = List.foldl (\char dict -> Dict.update char (\maybeVal -> Just (1 + Maybe.withDefault 0 maybeVal)) dict) Dict.empty answerList
        (finalMarks, _) =
            List.foldl
                (\( g, m ) ( marks, counts ) ->
                    case m of
                        2 ->
                            -- If the character is in the correct position, mark it as such and decrement the count.
                            ( ( g, m ) :: marks, Dict.update g (\maybeVal -> Maybe.map (\val -> val - 1) maybeVal) counts )

                        _ ->
                            -- If the character is not in the correct position, check if it exists elsewhere.
                            let
                                count = Maybe.withDefault 0 (Dict.get g counts)
                            in
                            if count > 0 then
                                -- If the character exists and hasn't been fully matched yet, mark it as present and decrement the count.
                                ( ( g, 1 ) :: marks, Dict.update g (\maybeVal -> Maybe.map (\val -> val - 1) maybeVal) counts )
                            else
                                -- If the character doesn't exist or has been fully matched, mark it as absent.
                                ( ( g, 0 ) :: marks, counts )
                )
                ([], answerCharCounts)
                (List.map2 Tuple.pair guess initialMarks)
    in
    List.reverse finalMarks

getLetterMarks : Model -> Dict Char Mark
getLetterMarks model =
    model.guesses
        |> List.concat
        |> List.foldl
            (\( char, mark ) dict ->
                Dict.update char
                    (\maybeMark ->
                        case maybeMark of
                            Just existingMark ->
                                Just (max existingMark mark)

                            Nothing ->
                                Just mark
                    )
                    dict
            )
            Dict.empty



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Attr.class "wordelm-app" ]
        [ heading
        , viewError model.error
        , viewBoard model
        , viewKeyboard model
        , viewMessage model.message
        ]

viewMessage : String -> Html Msg
viewMessage message =
    if String.isEmpty message then
        text ""
    else
        div
            [ Attr.class "message" ]
            [ text message ]


heading : Html Msg
heading =
    h1 [ Attr.class "heading" ] [ text "Wordelm" ]


viewError : String -> Html Msg
viewError error =
    div
        [ Attr.class "error-text" ]
        [ Html.text error ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        maxGuesses =
            6  -- Maximum number of guesses (rows)

        emptyGuess =
            List.repeat 5 (' ', 0)

        -- Create a list of rows for the guesses made so far
        guessRows =
            List.map viewRow model.guesses

        -- Calculate the number of empty rows needed to fill the board
        emptyRowsCount =
            maxGuesses - List.length model.guesses

        -- Create a list of empty rows
        emptyRows =
            List.repeat emptyRowsCount (viewRow emptyGuess)

        -- Combine the guess rows with the empty rows
        allRows =
            if model.gameState == Playing then
                -- If the game is still in progress, include the current guess row
                List.reverse guessRows ++ [viewCurrent model.current] ++ List.drop 1 emptyRows
            else
                -- If the game is over, just show the guess rows
                List.reverse guessRows ++ emptyRows
    in
    div
        [ Attr.class "board" ]
        allRows


viewRow : List ( Char, Mark ) -> Html Msg
viewRow guesses =
    div
        [ Attr.class "row" ]
        (List.map viewScoredTile guesses)


viewCurrent : List Char -> Html Msg
viewCurrent current =
    let
        emptyTilesCount =
            5 - List.length current

        emptyTiles =
            List.repeat emptyTilesCount (' ', 0)

        currentTiles =
            List.map (\char -> (char, -1)) (List.reverse current)  -- Use -1 for unknown mark

        fullRow =
            currentTiles ++ emptyTiles  -- Combine current tiles with empty tiles to make a full row
    in
    viewRow fullRow


viewScoredTile : ( Char, Mark ) -> Html Msg
viewScoredTile ( letter, mark ) =
    div
        [ Attr.class "tile" ]
        [ div
            [ Attr.class ("tile-" ++ toString mark) ]
            [ Html.text (String.fromChar (Char.toUpper letter)) ]
        ]


viewTile : Char -> Html Msg
viewTile letter =
    let
        tileContent =
            if letter == ' ' then
                ""
            else
                String.fromChar (Char.toUpper letter)
    in
    div
        [ Attr.class "tile" ]
        [ Html.text tileContent ]


viewKeyboard : Model -> Html Msg
viewKeyboard model =
    let
        marksDict =
            getLetterMarks model
    in
    div
        [ Attr.class "keyboard" ]
        (List.map (viewKeyboardRow marksDict) [ "QWERTYUIOP", "ASDFGHJKL", " ZXCVBNM<" ])


viewKeyboardRow : Dict Char Mark -> String -> Html Msg
viewKeyboardRow marksDict row =
    div
        [ Attr.class "keyboard-row" ]
        (List.map (viewKeyboardKey marksDict) (String.toList row))


translateKey : Char -> String
translateKey key =
    if key == ' ' then
        "Enter"

    else if key == '<' then
        "Backspace"

    else
        String.fromChar key


viewKeyboardKey : Dict Char Mark -> Char -> Html Msg
viewKeyboardKey marksDict key =
    let
        keyString = translateKey key
        markClass =
            case Dict.get (Char.toLower key) marksDict of
                Just mark ->
                    " keyboard-key-" ++ toString mark

                Nothing ->
                    ""
    in
    Html.button
        [ Attr.class ("keyboard-key" ++ markClass)
        , onClick (toKey keyString)
        ]
        [ Html.text keyString ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character (Char.toLower char)

        _ ->
            Control string
