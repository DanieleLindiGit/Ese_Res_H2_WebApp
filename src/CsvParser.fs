module CsvParser

open System
open Inputs

let ParseCsvLine (line: string) =
    let fields =
        line.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    // 01/01/1990 00:00
    // 0123456789
    let date = fields.[0].Trim()
    let day = date.Substring(0, 2)
    let month = date.Substring(3, 2)
    let hours = date.Substring(11, 2)

    let pvOut = fields.[1].Trim().Replace(',', '.')
    let windOut = fields.[2].Trim().Replace(',', '.')

    { PvWindNominalPower.Day = int day
      Month = int month
      Hours = int hours
      PvOut1MWh = float pvOut
      WindOut1MWh = float windOut }

let ParseText (text: string) =
    text.Split([| System.Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.skip 1
    |> Array.filter (fun line -> line.Length > 10)
    |> Array.map ParseCsvLine
    |> List.ofArray
