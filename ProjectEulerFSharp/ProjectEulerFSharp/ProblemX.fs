module ProblemX

open Problems
open Xunit
open Xunit.Abstractions


type Problems(output: ITestOutputHelper) =
    [<Fact>]
    member this.``Problem X``() =
        let max = 100_000

        let primes = findPrimesUpTo max
        let sum = primes |> List.sum

        output.WriteLine(
            sprintf "Number of primes below %d: %d" max (primes |> List.length)
        )

        output.WriteLine(sprintf "Sum of primes below %d: %d" max sum)
