module Problems

open Xunit
open Swensen.Unquote

// https://projecteuler.net/problem=1
[<Fact>]
let ``Problem 1: Multiples of 3 or 5`` () =
    let sum =
        [ 1..999 ] |> List.filter (fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum

    test <@ sum = 233168 @>

let rec fibonnaci n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fibonnaci (n - 1) + fibonnaci (n - 2)

let sumOfEvenFibonnaciTerms maxTerm =
    Seq.initInfinite id
    |> Seq.map fibonnaci
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.takeWhile (fun x -> x <= maxTerm)
    |> Seq.sum

[<Fact>]
let ``Fibonnaci sequence`` () =
    test <@ fibonnaci 0 = 1 @>
    test <@ fibonnaci 1 = 1 @>
    test <@ fibonnaci 2 = 2 @>
    test <@ fibonnaci 3 = 3 @>
    test <@ fibonnaci 4 = 5 @>
    test <@ fibonnaci 5 = 8 @>

// https://projecteuler.net/problem=2
[<Fact>]
let ``Problem 2: Even Fibonacci Numbers`` () =
    test <@ sumOfEvenFibonnaciTerms 55 = 44 @>
    test <@ sumOfEvenFibonnaciTerms 4_000_000 = 4613732 @>
