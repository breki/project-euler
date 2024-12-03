module Problems

open Xunit
open Swensen.Unquote

// https://projecteuler.net/problem=1
[<Fact>]
let ``Problem 1: Multiples of 3 or 5`` () =
    let sum =
        [ 1..999 ] |> List.filter (fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum

    test <@ sum = 233168 @>

let rec fibonnaci n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | n -> fibonnaci (n - 1) + fibonnaci (n - 2)

let sumOfEvenFibonnaciTerms maxTerm =
    Seq.initInfinite id
    |> Seq.map fibonnaci
    |> Seq.filter (fun x -> x % 2 = 0)
    |> Seq.takeWhile (fun x -> x <= maxTerm)
    |> Seq.sum

[<Fact>]
let ``Fibonnaci sequence`` () =
    test <@ fibonnaci 0 = 1 @>
    test <@ fibonnaci 1 = 1 @>
    test <@ fibonnaci 2 = 2 @>
    test <@ fibonnaci 3 = 3 @>
    test <@ fibonnaci 4 = 5 @>
    test <@ fibonnaci 5 = 8 @>

// https://projecteuler.net/problem=2
[<Fact>]
let ``Problem 2: Even Fibonacci Numbers`` () =
    test <@ sumOfEvenFibonnaciTerms 55 = 44 @>
    test <@ sumOfEvenFibonnaciTerms 4_000_000 = 4613732 @>

/// <summary>
/// Find a next prime that is greater than n.
/// </summary>
/// <returns>A tuple of the next prime found and a new list of primes
/// that includes this newly found prime.</returns>
let nextPrime (n, primes) =
    match n with
    | 1L -> (2L, [ 2L ])
    | n when n > 1L ->
        let isPrime m =
            primes |> Seq.exists (fun x -> m % x = 0L) |> not

        let m = Seq.initInfinite (fun i -> n + int64 i + 1L) |> Seq.find isPrime

        (m, m :: primes)
    | _ -> invalidArg "n" "n must be positive"


/// <summary>
/// Find all primes that are less or equal than n.
/// </summary>
/// <remarks>
/// If n is not a prime, the function will also include the next prime that
/// is greater than n.
/// </remarks>
let findPrimesUpTo n =
    let mutable m = 1L
    let mutable primes = []

    while m < n do
        let m', primes' = nextPrime (m, primes)
        m <- m'
        primes <- primes'

    primes


let h (n: int64) =
    // sequence from (n - 1) to 0
    match n with
    | 1L -> 1L
    | n when n > 1L ->
        seq { (n / 2L) .. -1L .. 0L } |> Seq.find (fun x -> n % x = 0)
    | _ -> invalidArg "n" "n must be positive"


[<Fact>]
let ``xxx`` () =
    test <@ h 1L = 1L @>
    test <@ h 2L = 1L @>
    test <@ h 6L = 3L @>
    test <@ h 10L = 5L @>
    test <@ h 100L = 50L @>
    test <@ h 1234567L = 9721L @>

    test <@ nextPrime (1, []) = (2, [ 2 ]) @>
    test <@ nextPrime (2, [ 2 ]) = (3, [ 3; 2 ]) @>
    test <@ nextPrime (3, [ 3; 2 ]) = (5, [ 5; 3; 2 ]) @>

    test <@ findPrimesUpTo 1L = [] @>
    test <@ findPrimesUpTo 2L = [ 2L ] @>
    test <@ findPrimesUpTo 3L = [ 3L; 2L ] @>
    test <@ findPrimesUpTo 4L = [ 5L; 3L; 2L ] @>
    test <@ findPrimesUpTo 5L = [ 5L; 3L; 2L ] @>

    test
        <@
            findPrimesUpTo 30L = [ 31L
                                   29L
                                   23L
                                   19L
                                   17L
                                   13L
                                   11L
                                   7L
                                   5L
                                   3L
                                   2L ]
        @>

    test <@ findPrimesUpTo 600851475143L |> List.length = 5 @>

// test <@ h 600851475143L = 500 @>
