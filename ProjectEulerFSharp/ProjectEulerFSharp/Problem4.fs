module Problem4

open Xunit
open Swensen.Unquote


let rec palindrome n =
    let rec palindromeRec m n =
        match n with
        | n when n > 0 ->
            let x = n % 10
            let nn = n / 10
            let mm = m * 10 + x

            palindromeRec mm nn
        | _ -> m

    n |> palindromeRec 0

// https://projecteuler.net/problem=4
[<Fact>]
let ``palindrome() function`` () =
    test <@ palindrome 0 = 0 @>
    test <@ palindrome 1 = 1 @>
    test <@ palindrome 10 = 1 @>
    test <@ palindrome 23 = 32 @>
    test <@ palindrome 1234 = 4321 @>
    test <@ palindrome 12345 = 54321 @>
