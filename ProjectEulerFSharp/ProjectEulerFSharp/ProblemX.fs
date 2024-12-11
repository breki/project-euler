module ProblemX

open Xunit
open Swensen.Unquote
open Problem10

type TD = { tn: int; ds: int list }

let triangleNumbers n : int list =
    let findNext (numbersSoFar: int list) (n: int) =
        let m = numbersSoFar |> List.head
        let o = m + n
        o :: numbersSoFar

    [ 2..n ] |> List.fold findNext [ 1 ]

let sieve = sieveOfEratosthenes 1000

let findSmallestPrimeDivisor n =
    let mutable m = 2
    let mutable divisor = None

    while divisor.IsNone do
        match sieve[m] with
        | 1uy -> if n % m = 0 then divisor <- Some m else ()
        | _ -> ()

        m <- m + 1

    divisor |> Option.get

let div n : Set<int> =
    let rec div' (d: Set<int>) n : Set<int> =
        match n with
        | 1 -> d |> Set.add 1
        | n ->
            let primeDivisor = findSmallestPrimeDivisor n
            let remaining = n / primeDivisor
            let d' = d |> Set.add primeDivisor
            div' d' remaining

    div' Set.empty n

type Problems() =
    [<Fact>]
    member this.``Find triangle numbers``() =
        test <@ triangleNumbers 1 = [ 1 ] @>
        test <@ triangleNumbers 2 = [ 3; 1 ] @>
        test <@ triangleNumbers 3 = [ 6; 3; 1 ] @>
        test <@ triangleNumbers 7 = [ 28; 21; 15; 10; 6; 3; 1 ] @>

    [<Fact>]
    member this.``Find divisors``() =
        test <@ div 1 = set [ 1 ] @>
        test <@ div 2 = set [ 1; 2 ] @>
        test <@ div 3 = set [ 1; 3 ] @>
        test <@ div 4 = set [ 1; 2; 4 ] @>
