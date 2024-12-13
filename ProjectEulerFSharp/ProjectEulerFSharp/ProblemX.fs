module ProblemX

open System
open Xunit
open Swensen.Unquote
open Problem10
open Xunit.Abstractions

type TD = { tn: int; ds: int list }

let triangleNumbers (n: int64) : int64 list =
    let findNext (numbersSoFar: int64 list) (n: int64) =
        let m = numbersSoFar |> List.head
        let o = m + n
        o :: numbersSoFar

    [ 2L .. n ] |> List.fold findNext [ 1L ]

let sieve = sieveOfEratosthenes 1000000

let findNextPrimeDivisor (startFrom: int64) (n: int64) =
    let mutable m = startFrom
    let mutable divisor = None

    while m <= n && divisor.IsNone do
        if m >= (Array.length sieve) || m >= Int32.MaxValue then
            sprintf "No divisor found for %d" n |> failwith
        else
            ()

        match sieve[int32 m] with
        | 1uy -> if n % m = 0 then divisor <- Some m else ()
        | _ -> ()

        m <- m + 1L

    divisor

let divisors (n: int64) : Set<int64> =
    let rec div' (d: Set<int64>) (n: int64) : Set<int64> =
        match n with
        | n when d |> Set.contains n -> d
        | 1L -> d |> Set.add 1
        | n ->
            let mutable d' = d

            for m in [2L .. n] do
                let primeDivisor = n |> findNextPrimeDivisor m

                match primeDivisor with
                | Some primeDivisor ->
                    let mutable divisor = primeDivisor
                    let mutable remaining = n

                    while remaining % primeDivisor = 0 do
                        remaining <- remaining / primeDivisor
                        d' <- (d' |> Set.add divisor) |> Set.union (div' d' remaining)
                        divisor <- divisor * primeDivisor

                    d' <- div' (d' |> Set.add n) remaining
                | None -> ()

            d'

    div' Set.empty n

type Problems(output: ITestOutputHelper) =
    [<Fact>]
    member this.``Find triangle numbers``() =
        test <@ triangleNumbers 1 = [ 1 ] @>
        test <@ triangleNumbers 2 = [ 3; 1 ] @>
        test <@ triangleNumbers 3 = [ 6; 3; 1 ] @>
        test <@ triangleNumbers 7 = [ 28; 21; 15; 10; 6; 3; 1 ] @>

    [<Fact>]
    member this.``Find divisors``() =
        // test <@ divisors 1 = set [ 1 ] @>
        // test <@ divisors 2 = set [ 1; 2 ] @>
        // test <@ divisors 3 = set [ 1; 3 ] @>
        // test <@ divisors 6 = set [ 1; 2; 3; 6 ] @>
        // test <@ divisors 10 = set [ 1; 2; 5; 10 ] @>
        // test <@ divisors 15 = set [ 1; 3; 5; 15 ] @>
        // test <@ divisors 21 = set [ 1; 3; 7; 21 ] @>
        // test <@ divisors 28 = set [ 1; 2; 4; 7; 14; 28 ] @>
        test <@ divisors 36 = set [ 1; 2; 3; 4; 6; 9; 12; 18; 36 ] @>

        // test
        //     <@
        //         divisors 120 = set
        //             [ 1; 2; 3; 4; 5; 6; 8; 10; 12; 15; 20; 24; 30; 40; 60; 120 ]
        //     @>

    // [<Fact(Skip = "Skip until divisors() works properly")>]
    [<Fact>]
    member this.``ProblemX``() =
        triangleNumbers 100
        |> List.rev
        |> List.map (fun t ->
            let d = divisors t
            (t, Set.count d, d))
        |> List.filter (fun (_, c, _) -> c > 50)
        |> List.iter (fun (t, c, ds) ->
            output.WriteLine(sprintf "%d: %d - %A" t c ds))
