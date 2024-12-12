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

let divisors n : Set<int> =
    let rec div' (d: Set<int>) n : Set<int> =
        match n with
        | 1 -> d |> Set.add 1
        | n ->
            let primeDivisor = findSmallestPrimeDivisor n

            let mutable divisor = primeDivisor
            let mutable remaining = n
            let mutable d' = d

            while remaining % primeDivisor = 0 do
                remaining <- remaining / primeDivisor
                d' <- Set.union d' (set [ divisor; remaining ])
                divisor <- divisor * primeDivisor

            div' d' remaining

    div' Set.empty n |> Set.add n

type Problems() =
    [<Fact>]
    member this.``Find triangle numbers``() =
        test <@ triangleNumbers 1 = [ 1 ] @>
        test <@ triangleNumbers 2 = [ 3; 1 ] @>
        test <@ triangleNumbers 3 = [ 6; 3; 1 ] @>
        test <@ triangleNumbers 7 = [ 28; 21; 15; 10; 6; 3; 1 ] @>

    [<Fact>]
    member this.``Find divisors``() =
        test <@ divisors 1 = set [ 1 ] @>
        test <@ divisors 2 = set [ 1; 2 ] @>
        test <@ divisors 3 = set [ 1; 3 ] @>
        test <@ divisors 6 = set [ 1; 2; 3; 6 ] @>
        test <@ divisors 10 = set [ 1; 2; 5; 10 ] @>
        test <@ divisors 15 = set [ 1; 3; 5; 15 ] @>
        test <@ divisors 21 = set [ 1; 3; 7; 21 ] @>
        test <@ divisors 28 = set [ 1; 2; 4; 7; 14; 28 ] @>
