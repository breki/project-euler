module Tests

open Xunit

[<Fact>]
let ``Problem 1: Multiples of 3 or 5`` () =
    let sum = [1..999] |> List.filter(fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum
    Assert.Equal(233168, sum)
