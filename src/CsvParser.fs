module CsvParser

open System
open Inputs
open Browser.Dom

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


let ParseArcGisLine (line:string) (factorOf1MW: float) =
    (*
        time,P,G(i),H_sun,T2m,WS10m,Int
        20150101:0010,0.0,0.0,0.0,-1.24,3.45,0.0
        yyyymmdd:hh--,POW,--,--,--,--,--
        01234567890
    *)
    let fields = line.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    let power =  sprintf "%.2f" (float fields.[1] / factorOf1MW)
    let powerString = power.Replace('.', ',')
    let dateString = 
        fields.[0].Substring(6,2) + "/" + 
        fields.[0].Substring(4, 2) + "/" +
        fields.[0].Substring(0, 4) + " " +
        fields.[0].Substring(9, 2) + ":00"
    sprintf "%s;%s;0" dateString powerString

let ParseArcGisFile (text: string) =
    // 1. Controllo che inizi con Latitude
    // 2. Cerco la riga -> Nominal power of the PV system (c-Si) (kWp):	1000.0
    // per vedere la potenza in kW
    // 3. Cerco la riga -> time,P,G(i),H_sun,T2m,WS10m,Int
    // per dividere le linee successive
    let allLines = text.Split([| System.Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries)
    let np = allLines |> Array.find (fun v -> v.StartsWith "Nominal power")
    let powerString = 
        np.Split([| ":" |], StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map (fun v -> v.Trim())
    let powerFactor = float powerString.[1] / 1_000_000.0
    let firstIdx = (allLines |> Array.findIndex (fun v -> v.StartsWith "time,P"))+1
    let lastIdx = firstIdx + 8760
    let dataLines = allLines.[firstIdx ..  lastIdx]

    let sb = System.Text.StringBuilder()
    sb.AppendLine("Date and Time;PV Output;Wind Output") |> ignore
    dataLines |> Array.iter (fun v ->
        let line = ParseArcGisLine v powerFactor
        sb.AppendLine(line) |> ignore)
    let el = document.getElementById "PvWindCsvData" :?> Browser.Types.HTMLInputElement
    el.value <- sb.ToString()

let LoadConfiguration (evt: Browser.Types.Event) =
  let elem = evt.target :?> Browser.Types.HTMLInputElement
  let f1 = elem.files.[0]
  let reader = FileReader.Create()
  reader.onload <- fun _ -> 
     let fileBuffer = (reader.result :?> string)
     ParseArcGisFile fileBuffer
  reader.readAsText f1  