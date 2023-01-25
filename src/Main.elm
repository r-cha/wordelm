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
        NoOp ->
            -- Do Nothing
            ( model, Cmd.none )

        Character char ->
            -- Process a letter being typed.
            if (List.length model.current < 5) && Char.isAlpha char then
                -- Only if there is a room and it's a letter
                ( { model | current = char :: model.current }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Control "Enter" ->
            -- Process a submission.
            -- This should only do anything if there are 5 letters available
            -- AND the guess is a word
            if List.length model.current == 5 then
                let
                    guess =
                        List.reverse model.current
                in
                if List.member (String.fromList guess) model.words then
                    ( { model
                        | current = []
                        , guesses = checkGuess guess model.answer :: model.guesses
                      }
                    , Cmd.none
                    )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

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
            ( { model | current = old }, Cmd.none )

        WordsLoaded (Ok words) ->
            let
                answer =
                    getTodaysWord model.now words
            in
            ( { model | words = words, answer = answer }, Cmd.none )

        WordsLoaded (Err _) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
        , viewBoard model
        , viewKeyboard
        ]


heading : Html Msg
heading =
    h1 [ Attr.class "heading" ] [ text "Wordelm" ]


viewBoard : Model -> Html Msg
viewBoard model =
    div
        [ Attr.class "board" ]
        (List.reverse (List.map viewRow model.guesses)
            ++ [ viewCurrent model.current ]
        )


viewRow : List ( Char, Mark ) -> Html Msg
viewRow guesses =
    div
        [ Attr.class "row" ]
        (List.map viewScoredTile guesses)


viewCurrent : List Char -> Html Msg
viewCurrent current =
    div
        [ Attr.class "row" ]
        (List.reverse (List.map viewTile current))


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
    div
        [ Attr.class "tile" ]
        [ Html.text (String.fromChar (Char.toUpper letter)) ]


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
