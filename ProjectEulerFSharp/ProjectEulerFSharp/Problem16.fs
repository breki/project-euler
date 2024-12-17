module Problem16

open System.Numerics
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let powerDigitSum n =
    let bigInt2 = BigInteger.Parse("2")

    let number = [ 1..n ] |> List.fold (fun p _ -> p * bigInt2) BigInteger.One


    let numberStr = number.ToString()

    numberStr
    |> Seq.toArray
    |> Array.fold
        (fun sum c ->
            let digit = int (c - '0')
            sum + digit)
        0

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``Power digit sum``() =
        test <@ powerDigitSum 0 = 1 @>
        test <@ powerDigitSum 1 = 2 @>
        test <@ powerDigitSum 15 = 26 @>
        test <@ powerDigitSum 1000 = 1366 @>
