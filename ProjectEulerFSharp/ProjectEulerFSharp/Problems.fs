module Problems

open Xunit
open Swensen.Unquote
open Xunit.Abstractions

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

        let oddNumbersAddition = if n % 2L = 0L then 1L else 2L

        let m =
            Seq.initInfinite (fun i ->
                // we only check odd numbers
                n + ((int64 i) * 2L) + oddNumbersAddition)
            |> Seq.find isPrime

        (m, m :: primes)
    | _ -> invalidArg "n" "n must be positive"


/// <summary>
/// Find all primes that are less or equal than n.
/// </summary>
let findPrimesUpTo n =
    let mutable m = 1L
    let mutable primes = []

    while m < n do
        let m', primes' = nextPrime (m, primes)
        m <- m'
        primes <- primes'

    if m <= n then primes else primes |> List.tail


/// <summary>
/// Find the highest prime that divides n.
/// </summary>
/// <remarks>The function is too slow.</remarks>
let highestPrimeOf n =
    let mutable n' = n

    let mutable highestPrimeSoFar = 1L
    let mutable primes = []

    while highestPrimeSoFar < n' && n' > 1 do
        let m', primes' = nextPrime (highestPrimeSoFar, primes)
        highestPrimeSoFar <- m'
        primes <- primes'

        while n' % highestPrimeSoFar = 0L do
            n' <- n' / highestPrimeSoFar

    highestPrimeSoFar



/// <summary>
/// Find the highest prime that divides n.
/// </summary>
/// <remarks>
/// The function is faster than highestPrimeOf() but it is still too slow
/// for the 600851475143 test case since it starts from the other way around
/// - it should have started from 2 and go up to n/2.
/// .</remarks>
let rec highestPrimeOfFaster (output: ITestOutputHelper) (n: int64) : int64 =
    output.WriteLine(sprintf "n: %d" n)

    match n with
    | n when n > 17 ->
        if n % (n / 2L) = 0L then
            highestPrimeOfFaster output (n / 2L)
        elif n % (n / 3L) = 0L then
            highestPrimeOfFaster output (n / 3L)
        elif n % (n / 5L) = 0L then
            highestPrimeOfFaster output (n / 5L)
        elif n % (n / 7L) = 0L then
            highestPrimeOfFaster output (n / 7L)
        elif n % (n / 11L) = 0L then
            highestPrimeOfFaster output (n / 11L)
        elif n % (n / 13L) = 0L then
            highestPrimeOfFaster output (n / 13L)
        elif n % (n / 17L) = 0L then
            highestPrimeOfFaster output (n / 17L)
        else
            let highestFactor =
                seq { (n / 17L) - 1L .. -1L .. 2L }
                |> Seq.tryFind (fun x -> n % x = 0)

            match highestFactor with
            // if a divisor is found, then we find the highest prime of the divisor
            | Some hh -> highestPrimeOfFaster output hh
            // if no divisor found, then n is a prime number
            | None -> n
    | 16L -> 2L
    | 15L -> 5L
    | 14L -> 7L
    | 12L -> 3L
    | 10L -> 5L
    | 9L -> 3L
    | 8L -> 2L
    | 6L -> 3L
    | 4L -> 2L
    | n when n > 0 -> n
    | _ -> invalidArg "n" "n must be positive"


/// <summary>
/// Find the largest prime factor of 600851475143L.
/// </summary>
[<Fact>]
let ``Problem 3: Largest Prime Factor`` () =
    // test <@ highestPrimeOfFaster 1L = 1L @>
    // test <@ highestPrimeOfFaster 2L = 2L @>
    // test <@ highestPrimeOfFaster 6L = 3L @>
    // test <@ highestPrimeOfFaster 10L = 5L @>
    // test <@ highestPrimeOfFaster 100L = 5L @>
    // test <@ highestPrimeOfFaster 1234567L = 9721L @>
    // test <@ highestPrimeOfFaster 35875456 = 280277L @>
    // test <@ highestPrimeOfFaster 135875456 = 1061527L @>
    // test <@ highestPrimeOfFaster 1135875456 = 4673L @>
    // test <@ highestPrimeOfFaster 11135875456L = 11903L @>

    // too slow:
    // test <@ highestPrimeOfFaster 111135875456L = 192133L @>
    // test <@ highestPrimeOfFaster 600851475143L = 6857 @>

    test <@ nextPrime (1, []) = (2, [ 2 ]) @>
    test <@ nextPrime (2, [ 2 ]) = (3, [ 3; 2 ]) @>
    test <@ nextPrime (3, [ 3; 2 ]) = (5, [ 5; 3; 2 ]) @>

    test <@ findPrimesUpTo 1L = [] @>
    test <@ findPrimesUpTo 2L = [ 2L ] @>
    test <@ findPrimesUpTo 3L = [ 3L; 2L ] @>
    test <@ findPrimesUpTo 4L = [ 3L; 2L ] @>
    test <@ findPrimesUpTo 5L = [ 5L; 3L; 2L ] @>

    test
        <@ findPrimesUpTo 30L = [ 29L; 23L; 19L; 17L; 13L; 11L; 7L; 5L; 3L; 2L ] @>

    test <@ highestPrimeOf 1 = 1 @>
    test <@ highestPrimeOf 2 = 2 @>
    test <@ highestPrimeOf 3 = 3 @>
    test <@ highestPrimeOf 4 = 2 @>
    test <@ highestPrimeOf 30 = 5 @>

// too slow:
// test <@ highestPrimeOf 35875456 = 280277L @>

type Problems(output: ITestOutputHelper) =
    [<Fact>]
    member this.``Problem 3: Largest Prime Factor``() =
        test <@ highestPrimeOfFaster output 100 = 5 @>
// test <@ highestPrimeOfFaster output 11135875456L = 11903L @>

// too slow:
// test <@ highestPrimeOfFaster output 111135875456L = 192133L @>
// test <@ highestPrimeOfFaster output 600851475143L = 280277L @>
