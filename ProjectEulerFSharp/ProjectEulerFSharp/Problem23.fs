module Problem23

open Xunit
open Swensen.Unquote
open Xunit.Abstractions
open Problem21


type NumberType =
    | Deficient
    | Perfect
    | Abundant

let max = 28123

let numbers =
    [| 0..max |]
    |> Array.map (fun i ->
        let sum = sumOfProperDivisors i

        match sum with
        | x when x < i -> Deficient
        | x when x > i -> Abundant
        | _ -> Perfect)

let abundantNumbers =
    numbers
    |> Array.mapi (fun i t -> (i, t))
    |> Array.filter (fun (i, t) -> t = Abundant)
    |> Array.map fst

let canBeWrittenAsSumOfTwoAbundantNumbers n =
    abundantNumbers
    |> Array.filter (fun i -> i < n - 1)
    |> Array.exists (fun i ->
        let j = n - i
        numbers[j] = Abundant)

let integersThatCannotBeWrittenAsSumOfTwoAbundantNumbers =
    [ 1..max ]
    |> List.map (fun i -> (i, canBeWrittenAsSumOfTwoAbundantNumbers i))
    |> List.filter (fun (_, x) -> not x)
    |> List.map fst

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``non-abundant sums``() =
        test <@ numbers[12] = Abundant @>
        test <@ numbers[28] = Perfect @>

        test <@ abundantNumbers |> Array.head = 12 @>

        test <@ canBeWrittenAsSumOfTwoAbundantNumbers 23 = false @>
        test <@ canBeWrittenAsSumOfTwoAbundantNumbers 24 = true @>

        test
            <@
                integersThatCannotBeWrittenAsSumOfTwoAbundantNumbers
                |> List.last = 20161
            @>

        let sum =
            integersThatCannotBeWrittenAsSumOfTwoAbundantNumbers |> List.sum

        test <@ sum = 4179871 @>
