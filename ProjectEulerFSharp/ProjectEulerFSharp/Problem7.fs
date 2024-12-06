module Problem7

open Problems
open Xunit
open Swensen.Unquote

let findNthPrime n =
    let mutable m = 1L
    let mutable primes = []

    while primes |> List.length < n do
        let m', primes' = nextPrime (m, primes)
        m <- m'
        primes <- primes'

    primes |> List.head

[<Fact>]
let ``Problem 7`` () =
    test <@ findNthPrime 6 = 13 @>
    test <@ findNthPrime 10001 = 104743 @>
