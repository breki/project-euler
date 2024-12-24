module Problem25

open System.Numerics
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let bigIntegerDigitsCount (n: BigInteger) = n.ToString().Length

let indexOfFirstFibonacciNumberWithDigitsCount l =
    match l with
    | 0 -> 1
    | 1 -> 1
    | l ->
            let mutable f = [| BigInteger.One; BigInteger.One |]

            Seq.initInfinite (
                fun i ->
                    let m = f[0] + f[1]
                    f[0] <- f[1]
                    f[1] <- m
                    m |> bigIntegerDigitsCount
                    )
            |> Seq.findIndex (fun x -> x >= l)
            |> (+) 3


type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``1000-digit Fibonacci number``() =
        test <@ indexOfFirstFibonacciNumberWithDigitsCount 1 = 1 @>
        test <@ indexOfFirstFibonacciNumberWithDigitsCount 2 = 7 @>
        test <@ indexOfFirstFibonacciNumberWithDigitsCount 3 = 12 @>
        test <@ indexOfFirstFibonacciNumberWithDigitsCount 10 = 45 @>
        test <@ indexOfFirstFibonacciNumberWithDigitsCount 1000 = 4782 @>
