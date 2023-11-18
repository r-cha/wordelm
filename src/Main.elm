module Main exposing (..)

-- Wordle clone in Elm because ✨ why not ✨

import Browser
import Browser.Events exposing (onKeyDown)
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


type alias Model =
    -- TODO: Add an error message field
    { current : List Char
    , guesses : List (List ( Char, Mark ))
    , answer : String
    , words : List String
    , now : Int
    , error : String
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


emptyModel : Int -> Model
emptyModel now =
    { current = []
    , guesses = []
    , answer = "OPALS"
    , words = []
    , now = now
    , error = ""
    }


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
            if (List.length model.current < 5) && Char.isAlpha char then
                -- Only if there is a room and it's a letter
                ( { model | current = char :: model.current, error = "" }
                , Cmd.none
                )

            else
                -- Truly ignore errant input - no message
                ( { model | error = "" }, Cmd.none )

        Control "Enter" ->
            -- Process a submission.
            if not (List.length model.current == 5) then
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
                    ( { model
                        | current = []
                        , guesses = checkGuess guess model.answer :: model.guesses
                        , error = ""
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
    -- TODO (maybe): make wordle-complete in terms of repeated guesses
    if guess == answer then
        ( guess, 2 )

    else if String.contains (String.fromChar guess) fullAnswer then
        ( guess, 1 )

    else
        ( guess, 0 )


checkGuess : List Char -> String -> List ( Char, Mark )
checkGuess guess answer =
    List.map
        (checkChars answer)
        (List.map2
            Tuple.pair
            guess
            (String.toList answer)
        )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Attr.class "wordelm-app" ]
        [ heading
        , viewError model.error
        , viewBoard model
        , viewKeyboard
        ]


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
            maxGuesses - List.length model.guesses - 1  -- Subtract 1 for the current guess row

        -- Create a list of empty rows
        emptyRows =
            List.repeat emptyRowsCount (viewRow emptyGuess)

        -- Combine the guess rows with the empty rows
        allRows =
            List.reverse guessRows ++ [viewCurrent model.current] ++ emptyRows  -- Include the current guess row
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


viewKeyboard : Html Msg
viewKeyboard =
    div
        [ Attr.class "keyboard" ]
        (List.map viewKeyboardRow [ "QWERTYUIOP", "ASDFGHJKL", " ZXCVBNM<" ])


viewKeyboardRow : String -> Html Msg
viewKeyboardRow row =
    div
        [ Attr.class "keyboard-row" ]
        (List.map viewKeyboardKey (String.toList row))


translateKey : Char -> String
translateKey key =
    if key == ' ' then
        "Enter"

    else if key == '<' then
        "Backspace"

    else
        String.fromChar key


viewKeyboardKey : Char -> Html Msg
viewKeyboardKey key =
    Html.button
        [ Attr.class "keyboard-key"
        , onClick (toKey (translateKey key))
        ]
        [ Html.text (translateKey key) ]



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
