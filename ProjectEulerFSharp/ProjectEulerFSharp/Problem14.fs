module Problem14

open Xunit
open Swensen.Unquote
open Xunit.Abstractions

let collatzSequence n =
    let rec c' s n =
        match n with
        | 1 -> 1 :: s
        | _ ->
            let m =
                if n % 2 = 0 then
                    n / 2
                else
                    3 * n + 1

            c' (n :: s) m

    (c' [] n) |> List.rev


type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``Collatz sequence``() =
        test <@ collatzSequence 13 = [ 13; 40; 20; 10; 5; 16; 8; 4; 2; 1 ] @>

    [<Fact>]
    member this.``Longest Collatz sequence``() =
        let cs =
            [1 .. 100000]
            |> List.map(fun n -> collatzSequence n |> List.length)
            |> List.sort
            |> List.rev

        let longest = cs |> List.head
        test <@ longest = 351  @>

