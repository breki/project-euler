module Problem8

open System
open System.Diagnostics
open Xunit
open Swensen.Unquote

let digitsOnly (s: string) =
    s.ToCharArray() |> Array.filter Char.IsDigit |> String

let digit n (s: string) : int = (s[n] - '0') |> int

type State =
    { pos: int
      product: int64
      maxProductSoFar: int64
      digits: int list }

let initialState n s =
    let digits = [ 0 .. (n - 1) ] |> List.map (fun i -> digit i s)
    let p = digits |> List.fold (fun p x -> p * (int64 x)) 1L

    { pos = 0
      product = p
      maxProductSoFar = p
      digits = digits }

let nextState state digitsCount (s: string) =
    let nextPos = state.pos + 1

    if nextPos + digitsCount >= s.Length then
        Debugger.Break()

    let nextDigit = digit (nextPos + digitsCount - 1) s
    let newDigits = [ nextDigit ] |> List.append (state.digits |> List.tail)
    let newProduct = newDigits |> List.fold (fun p x -> p * (int64 x)) 1L

    let newMaxProduct =
        if newProduct > state.maxProductSoFar then
            newProduct
        else
            state.maxProductSoFar

    { pos = nextPos
      product = newProduct
      maxProductSoFar = newMaxProduct
      digits = newDigits }

let largestProductInSeries digitsCount (series: string) =
    let finalState =
        [ 1 .. series.Length - digitsCount ]
        |> List.fold
            (fun state _ -> nextState state digitsCount series)
            (initialState digitsCount series)

    finalState.maxProductSoFar

let number1000 =
    @"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"

/// <summary>
/// Find the thirteen adjacent digits in the 1000-digit number that have
/// the greatest product.
/// </summary>
[<Fact>]
let ``Problem 8: Largest Product in a Series`` () =
    let series = number1000 |> digitsOnly
    test <@ series.Length = 1000 @>

    test <@ largestProductInSeries 4 series = 5832 @>
    test <@ largestProductInSeries 13 series = 23514624000L @>
