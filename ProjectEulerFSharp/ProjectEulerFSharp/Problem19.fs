module Problem19

open Xunit
open Swensen.Unquote
open Xunit.Abstractions


type Date =
    { Year: int
      Month: int
      DayOfMonth: int
      DayOfWeek: int }

    static member Of year month dayOfMonth dayOfWeek =
        { Year = year
          Month = month
          DayOfMonth = dayOfMonth
          DayOfWeek = dayOfWeek }


let daysOfMonth = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]

let isLeapYear year =
    (year % 4 = 0) && (not (year % 100 = 0) || (year % 400 = 0))

let nextDay (date: Date) : Date =
    let nextYear, nextMonth, nextDayOfMonth =
        match isLeapYear date.Year, date.Month with
        | true, 2 ->
            if date.DayOfMonth = 29 then
                date.Year, 3, 1
            else
                date.Year, 2, date.DayOfMonth + 1
        | _ ->
            if date.DayOfMonth = daysOfMonth[date.Month - 1] then
                match date.Month with
                | 12 -> date.Year + 1, 1, 1
                | _ -> date.Year, date.Month + 1, 1
            else
                date.Year, date.Month, date.DayOfMonth + 1

    Date.Of nextYear nextMonth nextDayOfMonth ((date.DayOfWeek + 1) % 7)

type Problems(_output: ITestOutputHelper) =
    [<Fact>]
    member this.``next day from the initial day``() =
        test <@ Date.Of 1900 1 1 0 |> nextDay = Date.Of 1900 1 2 1 @>

    [<Fact>]
    member this.``week day folds``() =
        test <@ Date.Of 1900 1 1 6 |> nextDay = Date.Of 1900 1 2 0 @>

    [<Fact>]
    member this.``month folds``() =
        test <@ Date.Of 1900 1 31 0 |> nextDay = Date.Of 1900 2 1 1 @>
        test <@ Date.Of 1900 2 28 0 |> nextDay = Date.Of 1900 3 1 1 @>
        test <@ Date.Of 1900 3 31 0 |> nextDay = Date.Of 1900 4 1 1 @>
        test <@ Date.Of 1900 4 30 0 |> nextDay = Date.Of 1900 5 1 1 @>
        test <@ Date.Of 1900 5 31 0 |> nextDay = Date.Of 1900 6 1 1 @>
        test <@ Date.Of 1900 6 30 0 |> nextDay = Date.Of 1900 7 1 1 @>
        test <@ Date.Of 1900 7 31 0 |> nextDay = Date.Of 1900 8 1 1 @>
        test <@ Date.Of 1900 8 31 0 |> nextDay = Date.Of 1900 9 1 1 @>
        test <@ Date.Of 1900 9 30 0 |> nextDay = Date.Of 1900 10 1 1 @>
        test <@ Date.Of 1900 10 31 0 |> nextDay = Date.Of 1900 11 1 1 @>
        test <@ Date.Of 1900 11 30 0 |> nextDay = Date.Of 1900 12 1 1 @>

    [<Fact>]
    member this.``February 29th``() =
        test <@ Date.Of 2000 2 28 0 |> nextDay = Date.Of 2000 2 29 1 @>
        test <@ Date.Of 2000 2 29 0 |> nextDay = Date.Of 2000 3 1 1 @>

    [<Fact>]
    member this.``year folds``() =
        test <@ Date.Of 1900 12 31 0 |> nextDay = Date.Of 1901 1 1 1 @>

    [<Fact>]
    member this.``leap years``() =
        test <@ 1900 |> isLeapYear = false @>
        test <@ 2000 |> isLeapYear = true @>
        test <@ 1904 |> isLeapYear = true @>
        test <@ 1903 |> isLeapYear = false @>

    [<Fact>]
    member this.``Counting Sundays - procedural``() =
        let mutable date = Date.Of 1900 1 1 0 |> Some
        let mutable sundays = 0

        while date.IsSome do
            if
                date.Value.Year >= 1901
                && date.Value.Year < 2001
                && date.Value.DayOfMonth = 1
                && date.Value.DayOfWeek = 6
            then
                sundays <- sundays + 1
            else
                ()

            date <- date.Value |> nextDay |> Some

            if date.Value.Year = 2001 then
                date <- None

        test <@ sundays = 171 @>

    [<Fact>]
    member this.``Counting Sundays - functional``() =
        let initialDate = Date.Of 1900 1 1 0

        let unfolder (date: Date) : (Date * Date) option =
            let nextDate = date |> nextDay

            if nextDate.Year <= 2000 then
                Some(date, nextDate)
            else
                None

        let sundays =
            Seq.unfold unfolder initialDate
            |> Seq.filter (fun date -> date.Year >= 1901 && date.Year < 2001)
            |> Seq.filter (fun date -> date.DayOfMonth = 1)
            |> Seq.filter (fun date -> date.DayOfWeek = 6)
            |> Seq.length

        test <@ sundays = 171 @>
