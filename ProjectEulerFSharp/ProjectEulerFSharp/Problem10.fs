module Problem10

open Xunit
open Xunit.Abstractions
open Swensen.Unquote

/// <summary>
/// A non-optimized implementation of the sieve of Eratosthenes.
/// </summary>
/// <remarks>For an optimization of the algorithm, see
/// https://projecteuler.net/overview=0010, for example.</remarks>
let sieveOfEratosthenes n =
    let sieve = Array.zeroCreate<byte> (n + 1)

    let markComposites p =
        sieve[p] <- 1uy
        [ p * 2 .. p .. n ] |> List.iter (fun i -> sieve.[i] <- 2uy)

    let processNumber i =
        match sieve[i] with
        | 2uy ->
            // it's a composite, do nothing
            ()
        | 1uy ->
            // this should never happen
            failwith "Already marked as prime"
        | 0uy ->
            // it'a a prime, mark all multiples as composite
            markComposites i
        | _ -> failwith "Invalid value"


    [| 2..n |] |> Array.iter processNumber

    sieve

let sumOfPrimes sieve =
    let mutable sum = 0L

    sieve
    |> Array.iteri (fun i x ->
        match x with
        | 1uy -> sum <- sum + (int64 i)
        | _ -> ())

    sum


type Problems(output: ITestOutputHelper) =
    /// <summary>
    /// Sum all primes below 2_000_000.
    /// </summary>
    [<Fact>]
    member this.``Problem 10``() =
        test <@ sieveOfEratosthenes 10 |> sumOfPrimes = 17 @>

        let max = 2_000_000
        let sum = sieveOfEratosthenes max |> sumOfPrimes

        test <@ sum = 142913828922L @>

        output.WriteLine(sprintf "Sum of primes below %d: %d" max sum)
