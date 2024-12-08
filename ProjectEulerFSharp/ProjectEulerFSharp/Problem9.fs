module Problem9

open Xunit
open Xunit.Abstractions


let findAB () =
    let findA a =
        [ a + 1 .. 1000 ]
        |> List.tryFind (fun b ->
            let c = 1000 - a - b
            a * a + b * b = c * c)

    [ 1..1000 ]
    |> List.map (fun a -> (a, findA a))
    |> List.filter (fun (_, b) -> b |> Option.isSome)
    |> List.map (fun (a, b) -> (a, b.Value))

type Problems(output: ITestOutputHelper) =
    /// <summary>
    /// Special Pythagorean Triplet
    /// </summary>
    [<Fact>]
    member this.``Problem 9``() =
        let candidates = findAB ()

        candidates
        |> List.iter (fun (a, b) ->
            let c = 1000 - a - b
            Assert.Equal(a * a + b * b, c * c)
            output.WriteLine(sprintf "%d² + %d² = %d²" a b c)

            let product = a * b * c
            output.WriteLine(sprintf "Product: %d" product))
