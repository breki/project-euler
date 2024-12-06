module Problem6

open Xunit
open Swensen.Unquote

let sumOfSquares n =
    [ 1..n ] |> List.map (fun x -> x * x) |> List.sum

let squaresSum n =
    [ 1..n ] |> List.sum |> (fun x -> x * x)

/// <summary>
/// Another naive solution, but I guess it conforms what was asked
/// and works fast enough, so I give it a pass.
/// </summary>
[<Fact>]
let ``Problem 6: Sum Square Difference`` () =
    test <@ squaresSum 10 - (sumOfSquares 10) = 2640 @>
    test <@ squaresSum 100 - (sumOfSquares 100) = 25164150 @>
