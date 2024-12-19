module Problem18_67

open System
open System.IO
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

type Triangle =
    { Depth: int
      Rows: int[,] }

    member this.Number (row: int) (column: int) : int =
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

    member this.Row(row: int) : int[] =
        [| for column in 0..row -> this.Number row column |]

    member this.Copy depth =
        let newTriangle =
            { Depth = depth
              Rows = Array2D.zeroCreate this.Depth this.Depth }

        Array2D.iteri
            (fun row column num -> newTriangle.Rows[row, column] <- num)
            this.Rows

        newTriangle

let parseTriangle (triangleDef: string) : Triangle =
    let triangleLines = triangleDef.Split(Environment.NewLine)
    let triangleDepth = triangleLines |> Seq.length

    let triangle =
        { Depth = triangleDepth
          Rows = Array2D.zeroCreate triangleDepth triangleDepth }

    triangleLines
    |> Array.map (fun line -> line.Split(" "))
    |> Array.map (fun lineNumsStr -> lineNumsStr |> Array.map Int32.Parse)
    |> Array.iteri (fun row rowNums ->
        rowNums
        |> Array.iteri (fun column num -> triangle.SetNumber row column num))

    triangle

let sumMaxPairs (rowIndex: int) (triangle: Triangle) =
    let maxBottomPair row column (triangle: Triangle) : int =
        let val1 = triangle.Number (row + 1) column
        let val2 = triangle.Number (row + 1) (column + 1)
        max val1 val2

    triangle.Row rowIndex
    |> Array.iteri (fun column number ->
        let sum =
            triangle.Number rowIndex column
            + (triangle |> maxBottomPair rowIndex column)

        triangle.SetNumber rowIndex column sum)

    triangle

let maxPathSum (triangle: Triangle) : int =
    seq { triangle.Depth - 2 .. -1 .. 0 }
    |> Seq.iter (fun row -> sumMaxPairs row triangle |> ignore)

    triangle.Number 0 0

let triangleDefSample =
    @"3
7 4
2 4 6
8 5 9 3"

let triangleDefProblem18 =
    @"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"


let triangleDefProblem67 = File.ReadAllText "data/0067_triangle.txt"

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing triangles``() =
        let triangle = parseTriangle triangleDefSample
        test <@ triangle.Depth = 4 @>
        test <@ triangle.Number 0 0 = 3 @>
        test <@ triangle.Number 1 1 = 4 @>
        test <@ triangle.Number 3 3 = 3 @>

    [<Fact>]
    member this.``reducing depth``() =
        let triangle = parseTriangle triangleDefSample

        let reduced = triangle |> sumMaxPairs (triangle.Depth - 2)

        test <@ reduced.Number 2 0 = 10 @>
        test <@ reduced.Number 2 1 = 13 @>
        test <@ reduced.Number 2 2 = 15 @>

    [<Fact>]
    member this.``Maximum Path Sum``() =
        test <@ triangleDefSample |> parseTriangle |> maxPathSum = 23 @>
        test <@ triangleDefProblem18 |> parseTriangle |> maxPathSum = 1074 @>

        let sum = triangleDefProblem67 |> parseTriangle |> maxPathSum
        test <@ sum = 23 @>
