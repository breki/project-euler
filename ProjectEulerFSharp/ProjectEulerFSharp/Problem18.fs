module Problem18

open System
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

type Triangle = {
    Depth: int
    Rows: int[,]
} with
    member this.Number(row: int) (column: int): int =
        if row < this.Depth then
            if column <= row then
                this.Rows[row, column]
            else
                invalidArg "column" "Column must be less than or equal to row"
        else
            invalidArg "row" "Row must be less than depth"

    member this.SetNumber (row: int) (column: int) value =
        if row < this.Depth then
            if column <= row then
                this.Rows[row, column] <- value
            else
                invalidArg "column" "Column must be less than or equal to row"
        else
            invalidArg "row" "Row must be less than depth"

    member this.Row (row: int) : int[] =
        [| for column in 0 .. row -> this.Number row column |]

    member this.Copy depth =
        let newTriangle =
            { Depth = depth
              Rows = Array2D.zeroCreate this.Depth this.Depth }
        Array2D.iteri (
            fun row column num -> newTriangle.Rows[row,column] <- num) this.Rows
        newTriangle

let parseTriangle (triangleDef: string) : Triangle =
    let triangleLines = triangleDef.Split(Environment.NewLine)
    let triangleDepth = triangleLines |> Seq.length

    let triangle = {
        Depth = triangleDepth
        Rows = Array2D.zeroCreate triangleDepth triangleDepth
    }

    triangleLines
    |> Array.map(fun line -> line.Split(" "))
    |> Array.map(fun lineNumsStr -> lineNumsStr |> Array.map Int32.Parse)
    |> Array.iteri (fun row rowNums -> rowNums |> Array.iteri (fun column num -> triangle.SetNumber row column num))

    triangle

let reduceTriangleDepth (triangle: Triangle) =
    let maxBottomPair row column (triangle: Triangle): int =
        let val1 = triangle.Number (row + 1) column
        let val2 = triangle.Number (row + 1) (column+1)
        max val1 val2

    let newDepth = triangle.Depth - 1
    let newFinalRowIndex = newDepth - 1
    let newTriangle = triangle.Copy newDepth

    triangle.Row newFinalRowIndex
    |> Array.iteri(fun column number ->
        let sum =
            triangle.Number newFinalRowIndex column
            + (triangle |> maxBottomPair newFinalRowIndex column)

        newTriangle.SetNumber newFinalRowIndex column sum
        )

    newTriangle


let triangleDefSample =
    @"3
7 4
2 4 6
8 5 9 3"

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing triangles``() =
        let triangle = parseTriangle triangleDefSample
        // test <@ triangle.Depth = 4 @>
        test <@ triangle.Number 0 0 = 3 @>
        test <@ triangle.Number 1 1 = 4 @>
        test <@ triangle.Number 3 3 = 3 @>

    [<Fact>]
    member this.``reducing depth``() =
        let triangle = parseTriangle triangleDefSample

        let reduced = triangle |> reduceTriangleDepth

        test <@ reduced.Depth = 3 @>
        test <@ reduced.Number 2 0 = 10 @>
        test <@ reduced.Number 2 1 = 13 @>
        test <@ reduced.Number 2 2 = 15 @>
