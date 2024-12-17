module Problem15

open System.Numerics
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let factorial n =
    [ 2 .. n ]
    |> List.map(fun x -> BigInteger(x))
    |> List.fold (fun product x -> product * x) BigInteger.One

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``Lattice paths``() =
        let n = 20

        let routesCount = (factorial (n * 2)) / (factorial n * factorial n )
        let routesCountStr = routesCount.ToString()

        test <@ routesCountStr = "137846528820" @>

