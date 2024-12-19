module ProblemTemplate

open Xunit
open Swensen.Unquote
open Xunit.Abstractions


type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``icebreaker``() =
        test <@ true @>
        test <@ true @>
