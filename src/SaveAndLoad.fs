module SaveAndLoad

open Browser.Dom

let DefSeparator = ";"
let BeginOfCSV = "[PHOTOVOLTAIC AND WIND BASE HOURLY PRODUCTION]"
let Instructions = """INSTRUCTIONS
# Apply the changes to this file.
# Once finished, from the menu under
# File -> Save As.. ->
# CSV (delimited by list separator) ( *.CSV )"""

let ClearFields () = 
   let allInpElements = document.querySelectorAll("input,textarea")
   [ 0 .. allInpElements.length - 1 ]
   |> List.iter (fun idx ->
      let el = allInpElements.[idx] :?> Browser.Types.HTMLInputElement
      if el.hasAttribute "data-uom" then el.value <- ""
   )

let SaveConfig () =
   let theForm = document.getElementById("mainform")
   let allInpElements = theForm.querySelectorAll("input,textarea,h4,h5")
   let sb = System.Text.StringBuilder(10000)
   sb.AppendLine(Instructions) |> ignore

   //TODO: si da per scontato che il csv sia l'ultimo elemento. Meglio identidicarlo per ID
   [ 0 .. allInpElements.length - 2 ]
   |> List.iter (fun idx ->
      let el = allInpElements.[idx] :?> Browser.Types.HTMLInputElement
      let uom = el.dataset.["uom"]
      let isTextArea = el.tagName = "TEXTAREA"
      let isHeader = el.tagName = "H4" || el.tagName = "H5"
      let take = el.hasAttribute "data-uom" || isHeader

      if take then
         if isHeader 
            then
               if el.tagName = "H4" then sb.AppendLine() |> ignore
               sb.Append("[").Append(el.innerText.ToUpper()).AppendLine("]") |> ignore
            else
               let v = el.value.Replace("\n", System.String.Empty)
               let u =
                  if not isTextArea && uom.Length > 0 then DefSeparator + uom else System.String.Empty
               sb.AppendLine(sprintf "%s%s%s%s" el.id DefSeparator v u) |> ignore
   )
   
   let csv = document.getElementById("PvWindCsvData") :?> Browser.Types.HTMLTextAreaElement
   sb.Append(csv.value) |> ignore

   //Download
   let link = document.createElement("a")
   link.setAttribute("style", "display: none;")
   link.setAttribute("target", "_blank")
   let universalBOM = "\uFEFF";
   link.setAttribute("href", "data:text/csv;charset=utf-8," + universalBOM + Fable.Core.JS.encodeURIComponent(sb.ToString()))
   link.setAttribute("download", "configuration.csv")
   document.body.appendChild(link) |> ignore
   link.click()
   document.body.removeChild(link) |> ignore

let SetValueKey (line:string) =
   let _array = line.Split([| DefSeparator |], System.StringSplitOptions.RemoveEmptyEntries)
   if _array.Length > 1 then
      let el = document.getElementById(_array.[0]) :?> Browser.Types.HTMLInputElement
      if el.hasAttribute "data-uom" then
         if el.tagName = "TEXTAREA" 
         then
            el.value <- _array |> Array.tail |> String.concat "; "
         elif el.classList.contains "option" && _array.Length = 2
            then el.value <- ""
         else el.value <- _array.[1]
         

let SetConfiguration (text:string) =
   let lines = text.Split([| "\r"; "\n" |], System.StringSplitOptions.RemoveEmptyEntries)
   let idxOfCSVDATA = lines |> Array.findIndex (fun v -> v.StartsWith BeginOfCSV)
   let data = lines.[0 .. idxOfCSVDATA-1]
   let csv = lines.[idxOfCSVDATA+1 ..] |> String.concat System.Environment.NewLine
   data |> Array.iter SetValueKey
   (document.getElementById("PvWindCsvData") :?> Browser.Types.HTMLTextAreaElement).value <- csv
   
let LoadConfiguration (evt: Browser.Types.Event) =
  ClearFields()
  let elem = evt.target :?> Browser.Types.HTMLInputElement
  let f1 = elem.files.[0]
  let reader = FileReader.Create()
  reader.onload <- fun _ -> 
     let fileBuffer = (reader.result :?> string)
     SetConfiguration fileBuffer
  reader.readAsText f1   