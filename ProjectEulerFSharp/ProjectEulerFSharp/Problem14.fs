module Problem14

open System
open System.Collections.Generic
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let collatzSequenceLength (d: Dictionary<int, int>) n =
    let rec c' s n =
        match n with
        | 1 -> s + 1
        | n when d.ContainsKey n -> s + d[n]
        | _ ->
            let m =
                if n % 2 = 0 then
                    n / 2
                else
                    3 * n + 1

            let len = c' (s + 1) m
            if d.ContainsKey n then
                raise (InvalidOperationException(sprintf "n: %d, d[n]: %d, len: %d" n d[n] len))
            else
                d[n] <- len
            len

    let len = c' 0 n

    if d.ContainsKey n then
        if d[n] <> len then
            raise (InvalidOperationException())
    else
        d[n] <- len

    len


type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``Collatz sequence length``() =
        let cache = Dictionary<int, int>()

        test <@ 13 |> collatzSequenceLength cache = 10 @>
        test <@ cache.Count = 9 @>
        test <@ cache[13] = 10 @>

    [<Fact>]
    member this.``Longest Collatz sequence``() =
        let max = 200000

        let cache = Dictionary<int, int>()
        let cs =
            [1 .. max]
            |> List.map(collatzSequenceLength cache)
            |> List.sort
            |> List.rev

        let longest = cs |> List.head
        test <@ longest = 351  @>

        test <@ cache.Count = max @>

