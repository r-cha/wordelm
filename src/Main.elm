module Main exposing (..)

-- Wordle clone in Elm because ✨ why not ✨

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Json.Decode as Decode

import Random.Extra as RE

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
    , scores : List Mark
    , answer : String
    }

type alias Mark = Int  -- 0=no, 1=yesish, 2=yes

todaysAnswer : String
todaysAnswer = "OPALS"  -- TODO: RE.choice ["WORKS", "FLAKE", "OPALS"]

emptyModel : Model
emptyModel = 
    { current = []
    , guesses = []
    , scores = []
    , answer = todaysAnswer
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
            if List.length model.current < 5 then
                ( { model | current = model.current ++ [char]}
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Control "Enter" ->
            -- Process a submission.
            -- This should only do anything if there are 5 letters available.
            if List.length model.current == 5 then
                let guess = String.fromList model.current
                in
                ( { model 
                    | current = []
                    , guesses = model.guesses ++ [guess]
                    , scores = model.scores ++ (checkGuess guess todaysAnswer)
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Control "Backspace" ->
            -- TODO: Not working :(
            -- Should just remove the last letter from current
            if List.length model.current > 0 then    
                ( { model | current = 
                    ( List.take 
                        ( List.length model.current - 1)
                        model.current
                    )
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


checkChars : (Char, Char) -> Mark
checkChars (guess, answer) =
    if guess == answer then
        2
    else
        if String.contains (String.fromChar guess) todaysAnswer then
            -- TODO: Remove dependence on global answer
            1
        else
            0

checkGuess : String -> String -> List Mark
checkGuess guess answer =
    List.map 
      checkChars
      ( List.map2 
          Tuple.pair
          (String.toList guess) (String.toList answer)
      )


-- VIEW

view : Model -> Html Msg
view model = 
    div
        []
        [ h1 [] [ text "Wordelm"]
        , h2 [] [ text "Previous: "]
        , viewGuesses model.guesses
        , h2 [] [ text "Typing: "]
        , viewCurrent model.current
        ]

viewCurrent : List Char -> Html Msg
viewCurrent current = 
    Html.text (String.fromList(current))

viewGuesses : List String -> Html Msg
viewGuesses guesses =
    Html.ul
        []
        (List.map viewGuess guesses)

viewGuess : String -> Html Msg
viewGuess guess = 
    -- TODO: Visualize score of guess (model.scores)
    Html.li
        []
        [ text guess ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
  case String.uncons string of
    Just (char, "") ->
      Character (Char.toUpper char)
    _ ->
      Control string