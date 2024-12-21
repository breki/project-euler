module Problem21

open Xunit
open Swensen.Unquote
open Xunit.Abstractions
open System

let divisors (n: int) =
    let x =
        [ 1 .. Math.Sqrt(n) |> int ]
        |> List.filter (fun i -> n % i = 0)
        |> Set.ofList

    let y = x |> Set.map (fun i -> n / i)

    x |> Set.union y

let properDivisors n = divisors n |> Set.remove n

let sumOfDivisors n =
    properDivisors n |> Array.ofSeq |> Array.sum

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``divisors``() =
        test <@ divisors 1 = set [ 1 ] @>
        test <@ divisors 2 = set [ 1; 2 ] @>
        test <@ divisors 3 = set [ 1; 3 ] @>
        test <@ divisors 6 = set [ 1; 2; 3; 6 ] @>
        test <@ divisors 10 = set [ 1; 2; 5; 10 ] @>
        test <@ divisors 15 = set [ 1; 3; 5; 15 ] @>
        test <@ divisors 21 = set [ 1; 3; 7; 21 ] @>
        test <@ divisors 28 = set [ 1; 2; 4; 7; 14; 28 ] @>
        test <@ divisors 36 = set [ 1; 2; 3; 4; 6; 9; 12; 18; 36 ] @>

        test
            <@
                divisors 120 = set
                    [ 1; 2; 3; 4; 5; 6; 8; 10; 12; 15; 20; 24; 30; 40; 60; 120 ]
            @>

        test
            <@
                divisors 220 = set
                    [ 1; 2; 4; 5; 10; 11; 20; 22; 44; 55; 110; 220 ]
            @>

    [<Fact>]
    member this.``amicable numbers``() =
        let max = 9999

        let divisorsSums = [| 0..max |] |> Array.map sumOfDivisors

        test <@ sumOfDivisors 220 = 284 @>
        test <@ sumOfDivisors 284 = 220 @>

        let isAmicable n =
            let n' = divisorsSums[n]
            if n' <= max then n' <> n && divisorsSums[n'] = n else false

        let amicableNumbers = [| 1..max |] |> Array.filter isAmicable

        amicableNumbers
        |> Array.iter (fun i -> sprintf "%d" i |> _output.WriteLine)

        let sum = [| 1..max |] |> Seq.filter isAmicable |> Seq.sum

        test <@ sum = 31626 @>
