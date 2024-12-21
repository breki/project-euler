module Problem20

open System.Numerics
open Problem15
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let ten = BigInteger.Parse("10")

let sumOfDigits (n: BigInteger) =
    let nextDigit (number: BigInteger) =
        if number.IsZero then
            None
        else
            let digit = int (number % ten)
            let remaining = number / ten
            Some(digit, remaining)

    let digits = Seq.unfold nextDigit n

    digits |> Seq.sum

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``sum of digits``() =
        test <@ sumOfDigits BigInteger.Zero = 0 @>
        test <@ sumOfDigits BigInteger.One = 1 @>
        test <@ BigInteger.Parse("10") |> sumOfDigits = 1 @>
        test <@ BigInteger.Parse("15") |> sumOfDigits = 6 @>
        test <@ BigInteger.Parse("123456789") |> sumOfDigits = 45 @>

    [<Fact>]
    member this.``factorial digit sum``() =
        test <@ factorial 100 |> sumOfDigits = 648 @>
