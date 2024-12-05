module Problem4

open Xunit
open Swensen.Unquote
open Xunit.Abstractions


let rec reverseDigits n =
    let rec reverseDigitsRec m n =
        match n with
        | n when n > 0 ->
            let x = n % 10
            let nn = n / 10
            let mm = m * 10 + x

            reverseDigitsRec mm nn
        | _ -> m

    n |> reverseDigitsRec 0

// https://projecteuler.net/problem=4
[<Fact>]
let ``reverseDigitsRec() function`` () =
    test <@ reverseDigits 0 = 0 @>
    test <@ reverseDigits 1 = 1 @>
    test <@ reverseDigits 10 = 1 @>
    test <@ reverseDigits 23 = 32 @>
    test <@ reverseDigits 1234 = 4321 @>
    test <@ reverseDigits 12345 = 54321 @>


let isPalindrome n = (n = reverseDigits n)

[<Fact>]
let ``isPalindrome() function`` () =
    test <@ isPalindrome 0 = true @>
    test <@ isPalindrome 1 = true @>
    test <@ isPalindrome 10 = false @>
    test <@ isPalindrome 11 = true @>
    test <@ isPalindrome 121 = true @>
    test <@ isPalindrome 122 = false @>
    test <@ isPalindrome 9009 = true @>
    test <@ isPalindrome 12321 = true @>
    test <@ isPalindrome 12345 = false @>

let largestPalindromeOf2DigitProduct (output: ITestOutputHelper) =
    let nn = [ 99..-1..0 ]
    let oo = [ 99..-1..0 ]

    let ll =
        [ for x in nn do
              for y in oo do
                  yield (x, y) ]

    ll
    |> List.map (fun (x, y) ->
        // output.WriteLine(sprintf "%d %d" x y)
        x * y)
    |> List.choose (fun p ->
        isPalindrome p
        |> function
            | true ->
                output.WriteLine(sprintf "%d" p)
                Some p
            | false -> None)
    |> List.max

let largestPalindromeOf3DigitProduct () =
    let nn = [ 999..-1..100 ]
    let oo = [ 999..-1..100 ]

    let ll =
        [ for x in nn do
              for y in oo do
                  yield (x, y) ]

    ll
    |> List.map (fun (x, y) -> x * y)
    |> List.choose (fun p ->
        isPalindrome p
        |> function
            | true -> Some p
            | false -> None)
    |> List.max


type Problems(output: ITestOutputHelper) =
    [<Fact>]
    member this.``Largest palindrome of product of two 2-digit numbers``() =
        test <@ largestPalindromeOf2DigitProduct output = 9009 @>

    [<Fact>]
    member this.``Problem 4: Largest palindrome of product of two 3-digit numbers``
        ()
        =
        test <@ largestPalindromeOf3DigitProduct () = 906609 @>
