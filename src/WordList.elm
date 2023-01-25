module WordList exposing (..)

import Http
import String


splitIntoWords : Result Http.Error String -> Result Http.Error (List String)
splitIntoWords result =
    result
        |> Result.map String.lines
        |> Result.map (List.map String.trim)


getWordList : Cmd (Result Http.Error (List String))
getWordList =
    Http.get
        { url = "https://raw.githubusercontent.com/tabatkins/wordle-list/main/words"
        , expect = Http.expectString splitIntoWords
        }


todaysWord : Int -> List String -> Maybe String
todaysWord date words =
    let
        dayZero =
            -- Roughly Jan 24 2023
            1674500400000

        msPerDay =
            86400000
    in
    List.take ((date - dayZero) // msPerDay) words
        |> List.reverse
        |> List.head


getTodaysWord : Int -> List String -> String
getTodaysWord date words =
    let
        s =
            todaysWord date words
    in
    case s of
        Just word ->
            word

        Nothing ->
            "OPALS"
