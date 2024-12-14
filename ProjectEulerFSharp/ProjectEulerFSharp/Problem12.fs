module Problem12

open System
open Xunit
open Swensen.Unquote
open Problem10
open Xunit.Abstractions

type TD = { tn: int; ds: int list }

/// <summary>
/// Return the first N triangle numbers.
/// </summary>
let triangleNumbers (n: int64) : int64 list =
    let findNext (numbersSoFar: int64 list) (n: int64) =
        let m = numbersSoFar |> List.head
        let o = m + n
        o :: numbersSoFar

    [ 2L .. n ] |> List.fold findNext [ 1L ]

let sieve = sieveOfEratosthenes 1000000

/// <summary>
/// Find the next prime divisor of a number, using the sieve of Eratosthenes.
/// </summary>
let findNextPrimeDivisor (startFrom: int64) (n: int64) =
    let mutable m = startFrom
    let mutable divisor = None

    while m * m <= n && divisor.IsNone do
        if m >= (Array.length sieve) || m >= Int32.MaxValue then
            sprintf "Looks like the sieve is too small to find the divisor for %d" n |> failwith
        else
            ()

        match sieve[int32 m] with
        | 1uy -> if n % m = 0 then divisor <- Some m else ()
        | _ -> ()

        m <- m + 1L

    divisor

/// <summary>
/// Find the divisors of a number using the sieve of Eratosthenes.
/// </summary>
let divisors (n: int64) : Set<int64> =
    let rec div' (d: Set<int64>) (n: int64) : Set<int64> =
        match n with
        | n when d |> Set.contains n -> d
        | 1L -> d |> Set.add 1
        | n ->
            let mutable m = Some 2L
            let mutable d' = d

            while m.IsSome && m.Value * m.Value <= n do
                let primeDivisor = n |> findNextPrimeDivisor m.Value

                match primeDivisor with
                | Some primeDivisor ->
                    let mutable divisor = primeDivisor
                    let mutable remaining = n

                    while remaining % primeDivisor = 0 do
                        remaining <- remaining / primeDivisor
                        d' <- (d' |> Set.add divisor) |> Set.union (div' d' remaining)
                        divisor <- divisor * primeDivisor

                    d' <- div' (d' |> Set.add n) remaining

                    match primeDivisor with
                    | 2L -> m <- Some 3
                    | _ -> m <- primeDivisor + 2L |> Some
                | None ->
                    m <- None

            d'

    div' (set [1L]) n

type Problems(_output: ITestOutputHelper) =
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
        test <@ divisors 36 = set [ 1; 2; 3; 4; 6; 9; 12; 18; 36 ] @>

        test
            <@
                divisors 120 = set
                    [ 1; 2; 3; 4; 5; 6; 8; 10; 12; 15; 20; 24; 30; 40; 60; 120 ]
            @>

    /// <summary>
    /// What is the value of the first triangle number to have over 500 divisors?
    /// </summary>
    [<Fact>]
    member this.``Problem 12``() =
        let x, _, _ =
            triangleNumbers 15000
            |> List.rev
            |> List.map (fun t ->
                let d = divisors t
                (t, Set.count d, d))
            |> List.find (fun (_, c, _) -> c >= 500)

        test <@ x = 76576500 @>
