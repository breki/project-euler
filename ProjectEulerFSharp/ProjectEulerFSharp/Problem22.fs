module Problem22

open System.IO
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let names =
    File.ReadAllText("data/0022_names.txt").Split(",")
    |> Array.map (_.Replace("\"", ""))
    |> Array.sort

let alphabeticalValue (s: string) =
    s.ToCharArray() |> Array.map (fun c -> ((c - 'A') |> int) + 1) |> Array.sum

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``names scores``() =
        test <@ names |> Array.head = "AARON" @>
        test <@ names |> Array.last = "ZULMA" @>

        test <@ "COLIN" |> alphabeticalValue = 53 @>

        let totalScores =
            names
            |> Array.mapi (fun i s -> (i + 1) * (s |> alphabeticalValue))
            |> Array.sum

        test <@ totalScores = 871198282 @>
