module Problem17

open System
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let rec numberToLetters n =
    let appendWithHyphen (b: string) (a: string) =
        match b with
        | "" -> a
        | b -> a + "-" + b

    match n with
    | 0 -> ""
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | n when n >= 20 && n < 30 ->
        "twenty" |> appendWithHyphen (numberToLetters (n - 20))
    | n when n >= 30 && n < 40 ->
        "thirty" |> appendWithHyphen (numberToLetters (n - 30))
    | n when n >= 40 && n < 50 ->
        "forty" |> appendWithHyphen (numberToLetters (n - 40))
    | n when n >= 50 && n < 60 ->
        "fifty" |> appendWithHyphen (numberToLetters (n - 50))
    | n when n >= 60 && n < 70 ->
        "sixty" |> appendWithHyphen (numberToLetters (n - 60))
    | n when n >= 70 && n < 80 ->
        "seventy" |> appendWithHyphen (numberToLetters (n - 70))
    | n when n >= 80 && n < 90 ->
        "eighty" |> appendWithHyphen (numberToLetters (n - 80))
    | n when n >= 90 && n < 100 ->
        "ninety" |> appendWithHyphen (numberToLetters (n - 90))
    | n when n < 1000 ->
        let hundreds = n / 100
        let remaining = n % 100

        match remaining with
        | 0 -> numberToLetters hundreds + " hundred"
        | remaining ->
            numberToLetters hundreds
            + " hundred and "
            + (numberToLetters remaining)
    | 1000 -> "one thousand"
    | _ -> raise (NotImplementedException())

let numberLetterCounts (numberText: string) =
    numberText.Replace(" ", "").Replace("-", "").Length

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``number to letters``() =
        test <@ numberToLetters 1 = "one" @>
        test <@ numberToLetters 2 = "two" @>
        test <@ numberToLetters 3 = "three" @>
        test <@ numberToLetters 4 = "four" @>
        test <@ numberToLetters 5 = "five" @>
        test <@ numberToLetters 6 = "six" @>
        test <@ numberToLetters 7 = "seven" @>
        test <@ numberToLetters 8 = "eight" @>
        test <@ numberToLetters 9 = "nine" @>
        test <@ numberToLetters 10 = "ten" @>
        test <@ numberToLetters 11 = "eleven" @>
        test <@ numberToLetters 12 = "twelve" @>
        test <@ numberToLetters 13 = "thirteen" @>
        test <@ numberToLetters 14 = "fourteen" @>
        test <@ numberToLetters 15 = "fifteen" @>
        test <@ numberToLetters 16 = "sixteen" @>
        test <@ numberToLetters 17 = "seventeen" @>
        test <@ numberToLetters 18 = "eighteen" @>
        test <@ numberToLetters 19 = "nineteen" @>
        test <@ numberToLetters 20 = "twenty" @>
        test <@ numberToLetters 21 = "twenty-one" @>
        test <@ numberToLetters 29 = "twenty-nine" @>
        test <@ numberToLetters 30 = "thirty" @>
        test <@ numberToLetters 31 = "thirty-one" @>
        test <@ numberToLetters 40 = "forty" @>
        test <@ numberToLetters 42 = "forty-two" @>
        test <@ numberToLetters 50 = "fifty" @>
        test <@ numberToLetters 51 = "fifty-one" @>
        test <@ numberToLetters 60 = "sixty" @>
        test <@ numberToLetters 61 = "sixty-one" @>
        test <@ numberToLetters 70 = "seventy" @>
        test <@ numberToLetters 71 = "seventy-one" @>
        test <@ numberToLetters 80 = "eighty" @>
        test <@ numberToLetters 81 = "eighty-one" @>
        test <@ numberToLetters 90 = "ninety" @>
        test <@ numberToLetters 91 = "ninety-one" @>
        test <@ numberToLetters 100 = "one hundred" @>
        test <@ numberToLetters 101 = "one hundred and one" @>
        test <@ numberToLetters 142 = "one hundred and forty-two" @>
        test <@ numberToLetters 912 = "nine hundred and twelve" @>
        test <@ numberToLetters 942 = "nine hundred and forty-two" @>
        test <@ numberToLetters 1000 = "one thousand" @>

    [<Fact>]
    member this.``Number letter counts``() =
        test <@ numberToLetters 342 |> numberLetterCounts = 23 @>
        test <@ numberToLetters 115 |> numberLetterCounts = 20 @>

        let sum =
            [ 1..1000 ]
            |> List.map numberToLetters
            |> List.map numberLetterCounts
            |> List.sum

        test <@ sum = 21124 @>
