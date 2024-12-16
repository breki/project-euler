module Problem14

open System.Collections.Generic
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let collatzSequenceLengthRec (d: Dictionary<int, int>) (n: int32) =
    let rec c' s (m: int64) =
        match m with
        | 1L -> s + 1
        | m when m <= n && d.ContainsKey (int m) -> s + d[(int m)]
        | _ ->
            let o: int64 =
                if m % 2L = 0L then
                    m / 2L
                else
                    3L * m + 1L

            c' (s + 1) o

    let len = c' 0 n
    d[n] <- len
    len

let collatzSequenceLengthLoop (d: Dictionary<int, int>) (n: int) =
    let mutable (m: int64) = n
    let mutable lengthSoFar = 1

    while m <> 1 do
        if m <= n && d.ContainsKey (int32 m) then
            lengthSoFar <- lengthSoFar + d[int32 m] - 1
            m <- 1
        else
            let m1 =
                if m % 2L = 0 then
                    m / 2L
                else
                    3L * m + 1L
            m <- m1
            lengthSoFar <- lengthSoFar + 1

    d[n] <- lengthSoFar
    lengthSoFar


type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``Collatz sequence length (recursive)``() =
        let cache = Dictionary<int, int>()
        test <@ 13 |> collatzSequenceLengthRec cache = 10 @>
        test <@ cache[13] = 10 @>
        test <@ 160 |> collatzSequenceLengthRec cache = 11 @>
        test <@ 60975 |> collatzSequenceLengthRec cache = 335 @>
        test <@ 77031 |> collatzSequenceLengthRec cache = 351 @>

    [<Fact>]
    member this.``Collatz sequence length (loop)``() =
        let cache = Dictionary<int, int>()
        test <@ 13 |> collatzSequenceLengthLoop cache = 10 @>
        test <@ cache[13] = 10 @>
        test <@ 160 |> collatzSequenceLengthLoop cache = 11 @>
        test <@ 60975 |> collatzSequenceLengthLoop cache = 335 @>
        test <@ 77031 |> collatzSequenceLengthLoop cache = 351 @>

    [<Fact>]
    member this.``Longest Collatz sequence (recursive)``() =
        let max = 1000000

        let cache = Dictionary<int, int>()
        let cs =
            [1 .. max]
            |> List.map(fun n -> (n, collatzSequenceLengthRec cache n))
            |> List.sortBy snd
            |> List.rev

        let longest = cs |> List.head
        test <@ longest = (837799, 525)  @>

        test <@ cache.Count = max @>

    [<Fact>]
    member this.``Longest Collatz sequence (loop)``() =
        let max = 1000000

        let cache = Dictionary<int, int>()
        let cs =
            [1 .. max]
            |> List.map(fun n -> (n, collatzSequenceLengthLoop cache n))
            |> List.sortBy snd
            |> List.rev

        let longest = cs |> List.head
        test <@ longest = (837799, 525)  @>

        test <@ cache.Count = max @>

