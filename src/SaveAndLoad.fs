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
      if el.value.Length > 0 then
         let v = el.value.Replace("\n", " ")
         sb.AppendLine(sprintf "%s = %s" el.id v) |> ignore
   )
   sb.AppendLine()
      .AppendLine("#CSVDATA#") |> ignore
   
   let csv = document.getElementById("PvWindCsvData") :?> Browser.Types.HTMLTextAreaElement
   sb.Append(csv.value) |> ignore

   //Download
   let link = document.createElement("a")
   link.setAttribute("style", "display: none;")
   link.setAttribute("target", "_blank")
   link.setAttribute("href", "data:text/plain;charset=utf-8," + Fable.Core.JS.encodeURIComponent(sb.ToString()))
   link.setAttribute("download", "configuration.txt")
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