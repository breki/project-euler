module Problem24

open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let rec lexicographicPermutations
    (digitsAvailable: string)
    (numberSoFar: string)
    =
    if digitsAvailable.Length > 0 then
        [| 0 .. digitsAvailable.Length - 1 |]
        |> Array.map (fun i ->
            let t' = numberSoFar + digitsAvailable[i].ToString()
            let n' = digitsAvailable.Remove(i, 1)
            let x = lexicographicPermutations n' t'
            x)
        |> Array.concat
    else
        [| numberSoFar |]

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``lexicographic permutations``() =
        test
            <@
                lexicographicPermutations "012" "" = [| "012"
                                                        "021"
                                                        "102"
                                                        "120"
                                                        "201"
                                                        "210" |]
            @>

        let x = lexicographicPermutations "0123456789" ""

        test <@ x |> Array.length = 3628800 @>
        test <@ x[1_000_000 - 1] = "2783915460" @>
