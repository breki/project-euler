module Problem5

open Xunit
open Swensen.Unquote
open Xunit.Abstractions

/// <summary>
/// Find the smallest number that can be divided by each of the
/// divisorsToCheckFor without any remainder.
/// </summary>
/// <remarks>
/// Note that this is a brute force solution, not really in the spirit
/// of Project Euler.
/// </remarks>
let smallestMultiple divisorsToCheckFor minValueToSearchFrom =
    Seq.initInfinite (fun i -> minValueToSearchFrom + i * 2)
    |> Seq.find (fun x ->
        divisorsToCheckFor |> List.forall (fun i -> x % i = 0))

type Problems(output: ITestOutputHelper) =

    [<Fact>]
    member this.``xxx``() =
        let minValueToSearchFrom = 2 * 3 * 5 * 7
        let divisorsToCheckFor = [ 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

        let x = smallestMultiple divisorsToCheckFor minValueToSearchFrom
        test <@ x = 2520 @>

    [<Fact>]
    member this.``xxx2``() =
        let minValueToSearchFrom = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

        let divisorsToCheckFor =
            [ 2
              3
              4
              5
              6
              7
              8
              9
              10
              11
              12
              13
              14
              15
              16
              17
              18
              19
              20 ]

        let x = smallestMultiple divisorsToCheckFor minValueToSearchFrom
        test <@ x = 232792560 @>
