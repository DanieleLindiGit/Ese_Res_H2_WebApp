module SaveAndLoad

open Browser.Dom

let ClearFields () = 
   let allInpElements = document.querySelectorAll("input,textarea")
   [ 0 .. allInpElements.length - 1 ]
   |> List.iter (fun idx ->
      let el = allInpElements.[idx] :?> Browser.Types.HTMLInputElement
      el.value <- ""
   )

let SaveConfig () =
   let allInpElements = document.querySelectorAll("input,textarea")
   let sb = System.Text.StringBuilder(10000)

   //TODO: si da per scontato che il csv sia l'ultimo elemento. Meglio identidicarlo per ID
   [ 0 .. allInpElements.length - 2 ]
   |> List.iter (fun idx ->
      let el = allInpElements.[idx] :?> Browser.Types.HTMLInputElement
      let uom = el.dataset.["uom"]
      let isTextArea = el.tagName = "TEXTAREA"
      let take = el.hasAttribute "data-uom"

      if take then
         let v = el.value.Replace("\n", System.String.Empty)
         let u =
            if not isTextArea && uom.Length > 0 then ";"+uom else System.String.Empty
         sb.AppendLine(sprintf "%s;%s%s" el.id v u) |> ignore
   )

   sb.AppendLine()
      .AppendLine()
      .AppendLine("#CSVDATA#") |> ignore
   
   printfn "%s" (sb.ToString())
   (*
   let csv = document.getElementById("PvWindCsvData") :?> Browser.Types.HTMLTextAreaElement
   sb.Append(csv.value) |> ignore
   *)

   //Download
   let link = document.createElement("a")
   link.setAttribute("style", "display: none;")
   link.setAttribute("target", "_blank")
   link.setAttribute("href", "data:text/csv;charset=utf-8," + Fable.Core.JS.encodeURIComponent(sb.ToString()))
   link.setAttribute("download", "configuration.csv")
   document.body.appendChild(link) |> ignore
   link.click()
   document.body.removeChild(link) |> ignore
   

let SetConfiguration (text:string) =
   let lines = text.Split([| System.Environment.NewLine |], System.StringSplitOptions.RemoveEmptyEntries)
   let idxOfCSVDATA = lines |> Array.findIndex (fun v -> v.StartsWith "#CSVDATA#")
   let data = lines.[0 .. idxOfCSVDATA-1]
   let csv = lines.[idxOfCSVDATA+1 ..] |> String.concat System.Environment.NewLine
   data |> Array.iter (fun line -> 
      let temp = line.Split([| "=" |], System.StringSplitOptions.RemoveEmptyEntries)
      let _key = temp.[0].Trim()
      let _value = temp.[1].Trim()
      if _key.Length > 1 then
         (document.getElementById(_key) :?> Browser.Types.HTMLInputElement).value <- _value
      )
   (document.getElementById("PvWindCsvData") :?> Browser.Types.HTMLTextAreaElement).value <- csv
   
let LoadConfiguration (evt: Browser.Types.Event) =
  //ClearFields()
  let elem = evt.target :?> Browser.Types.HTMLInputElement
  let f1 = elem.files.[0]
  let reader = FileReader.Create()
  reader.onload <- fun _ -> 
     let fileBuffer = (reader.result :?> string)
     SetConfiguration fileBuffer
  reader.readAsText f1   