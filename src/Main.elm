module Main exposing (..)

-- Wordle clone in Elm because ✨ why not ✨

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode as Decode



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { current : List Char
    , guesses : List String
    , scores : List (List Mark)
    , answer : String

    -- TODO: Use a single 5x6 array
    -- TODO: Add an error message field
    }


type alias Mark =
    Int



-- 0=no, 1=yesish, 2=yes


emptyModel : Model
emptyModel =
    { current = []
    , guesses = []
    , scores = []
    , answer = "OPALS" -- TODO: RE.choice ["WORKS", "FLAKE", "OPALS"]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Maybe.withDefault emptyModel Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Character Char
    | Control String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Character char ->
            -- Process a letter being typed.
            -- It should be added to model.current if there is room
            -- TODO: More character filtering
            if List.length model.current < 5 then
                ( { model | current = char :: model.current }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Control "Enter" ->
            -- Process a submission.
            -- This should only do anything if there are 5 letters available.
            if List.length model.current == 5 then
                let
                    guess =
                        String.reverse (String.fromList model.current)
                in
                ( { model
                    | current = []
                    , guesses = guess :: model.guesses
                    , scores = checkGuess guess model.answer :: model.scores
                  }
                , Cmd.none
                )

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

        _ ->
            ( model, Cmd.none )


checkChars : String -> ( Char, Char ) -> Mark
checkChars fullAnswer ( guess, answer ) =
    if guess == answer then
        2

    else if String.contains (String.fromChar guess) fullAnswer then
        1

    else
        0


checkGuess : String -> String -> List Mark
checkGuess guess answer =
    List.map
        (checkChars answer)
        (List.map2
            Tuple.pair
            (String.toList guess)
            (String.toList answer)
        )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Attr.class "wordelm-app" ]
        [ heading
        , viewBoard model
        , viewKeys
        ]


heading : Html Msg
heading =
    h1 [ Attr.class "heading" ] [ text "Wordelm" ]


viewBoard : Model -> Html Msg
viewBoard model =
    div
        [ Attr.class "board" ]
        [ viewRow model 0
        , viewRow model 1
        , viewRow model 2
        , viewRow model 3
        , viewRow model 4
        , viewRow model 5
        ]


viewRow : Model -> Int -> Html Msg
viewRow model index =
    div
        [ Attr.class "row" ]
        [ viewTile 'R' 0
        , viewTile 'O' 1
        , viewTile 'C' 2
        ]


viewTile : Char -> Mark -> Html Msg
viewTile letter mark =
    div
        [ Attr.class "tile" ]
        [ Html.text (String.fromChar letter) ]


viewKeys : Html Msg
viewKeys =
    div
        [ Attr.class "keyboard" ]
        [ Html.p [ Attr.class "keyboard-row" ] [ Html.text "QWERTYUIOP" ]
        , Html.p [ Attr.class "keyboard-row" ] [ Html.text "ASDFGHJKL" ]
        , Html.p [ Attr.class "keyboard-row" ] [ Html.text " ZXCVBNM<" ]
        ]


viewCurrent : List Char -> Html Msg
viewCurrent current =
    Html.text (String.reverse (String.fromList current))


viewGuesses : List String -> Html Msg
viewGuesses guesses =
    Html.ul
        [ Attr.class "guess-list" ]
        (List.reverse (List.map viewGuess guesses))


viewGuess : String -> Html Msg
viewGuess guess =
    -- TODO: Visualize score of guess (model.scores)
    Html.li
        [ Attr.class "guess" ]
        [ text guess ]



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
            Character (Char.toUpper char)

        _ ->
            Control string
