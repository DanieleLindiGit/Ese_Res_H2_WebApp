module HtmlFormatters

open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
open Outputs

[<Emit("new Intl.NumberFormat('it-IT', { style: 'decimal' }).format($0)")>]
let decimalFormatter (value: float) : string = jsNative

[<Emit("new Intl.NumberFormat('it-IT', { style: 'decimal', maximumFractionDigits: 0 }).format($0)")>]
let intFormatter (value: float) : string = jsNative

let sprintloc format value =
   (sprintf format value).ToString().Replace('.', ',')

let sprintloc2 format value1 value2 =
   (sprintf format value1 value2).ToString().Replace('.', ',')

let sprintloc3 format value1 value2 value3 =
   (sprintf format value1 value2 value3).ToString().Replace('.', ',')

let sprintloc4 format value1 value2 value3 value4=
   (sprintf format value1 value2 value3 value4).ToString().Replace('.', ',')

let writeHeader (desc: string) =
   sprintf "<th>%s</th>" desc

let writeintCol (x : int) (s: System.Text.StringBuilder) =
   s.Append(sprintf "<td>%i</td>" x)

let write0decCol (x : float) (s: System.Text.StringBuilder)  =
   s.Append(sprintf "<td>%.0f</td>" x)

let write2decCol (x : float) (s: System.Text.StringBuilder) =
   s.Append(sprintloc "<td>%.2f</td>" x)

let getTableDownloadButton tableId =
   sprintf 
     """
     <div class="text-end">
            <a class="btn btn-secondary" href="#" onclick="download_table_as_csv('%s');">
              Download
              <span data-feather="download"></span>
            </a>
          </div> 
     """ tableId

let createBattery (batteryOutput: BatteryOutput list) =
   let el = document.getElementById("batteryBody")
   el.innerHTML <- ""
   for b in batteryOutput do
      let row = document.createElement("tr") :?> Browser.Types.HTMLTableRowElement
      row.innerHTML <- sprintloc3 "<td>%i</td><td>%.2f</td><td>%.2f</td>" b.YearOfOperation b.MWh b.MW
      el.appendChild row |> ignore
      
let createBiomass (biomass: BiomassGasifierOutput) =
   let el = document.getElementById("biomassBody")
   el.innerHTML <- 
      sprintloc4 """<dl class="row">
      <dt class="col-sm-3">Thermal Power</dt><dd class="col-sm-9">%.0f MWth</dd>
      <dt class="col-sm-3">Max Biomass consumption 1h</dt><dd class="col-sm-9">%.1f t/h</dd>
      <dt class="col-sm-3">Biomass Consumption</dt><dd class="col-sm-9">%.1f kgBiomass/kWh</dd>
      <dt class="col-sm-3">Minimum Load</dt><dd class="col-sm-9">%.1f Mwel</dd>
      </dl>""" biomass.ThermalPower biomass.MaxBiomassConsumptionOneHour biomass.BiomassConsumption biomass.MinimumLoad

let formatEnergyDegradationOverYears (ed: EnergyDegradationOverYears) =
   let el = document.getElementById(ed.SerieName + "Div")
   let sb = System.Text.StringBuilder(10000)
   let tableId = ed.SerieName + "Data"
   sb.AppendLine (sprintf "<h3>%s with Degradation</h3><hr>" ed.SerieName) |> ignore
   sb.AppendLine (getTableDownloadButton tableId) |> ignore
   sb.AppendLine 
      ("<table id=\"" 
      + tableId 
      + """" class="table table-striped">
           <thead><tr>
              <th>Year</th>
              <th>Total</th>
              <th>Minimum</th>
              <th>Maximum</th>
              <th>Average</th>
              </tr>
              <tr>
                <th>#</th>
                <th>kW</th>
                <th>kW</th>
                <th>kW</th>
                <th>kW</th>
              </tr>
            </thead>
            <tbody>""") |> ignore
   for (year, total, min, max, avg) in ed.Rows do
      sb.AppendLine (sprintf "<tr><td>%i</td><td>%s</td><td>%.0f</td><td>%.0f</td><td>%.0f</td></tr>" 
      year 
      (intFormatter total) 
      min 
      max 
      avg) 
      |> ignore
   sb.AppendLine "</tbody></table>" |> ignore
   el.innerHTML <- sb.ToString()

let createElectrolyzers (output: ElectrolyzersOutput) =
   let el = document.getElementById("ElectrolyzersDiv")
   let sb = System.Text.StringBuilder(10000)
   sb.AppendLine("<h3>Electrolyzers Data Output</h3><hr>")
     .AppendLine("""<dl class="row">""")
     .AppendLine(sprintloc """<dt class="col-sm-3">Power DC Consumption Total</dt> <dd class="col-sm-9">%.1f MW</dd>""" output.PowerDcConsumptionTot)
     .AppendLine(sprintloc """<dt class="col-sm-3">Nominal H2 Production Total</dt> <dd class="col-sm-9">%.0f kgH2/h</dd>""" output.NominalH2ProductionTot)
     .AppendLine(sprintloc """<dt class="col-sm-3">WaterConsumption Total</dt> <dd class="col-sm-9">%.4f tonH20/kgH2</dd>""" output.WaterConsumption)
     .AppendLine(sprintloc """<dt class="col-sm-3">Water Discharged Total</dt> <dd class="col-sm-9">%.3f tonH20/kgH2</dd>""" output.WaterDischarged)
     .AppendLine(sprintloc """<dt class="col-sm-3">Oxygen Production Total</dt> <dd class="col-sm-9">%.0f kgO2/kgH2</dd>""" output.OxygenProduction)
     .AppendLine(sprintloc """<dt class="col-sm-3">Hydrogen Specific Volume NTP</dt> <dd class="col-sm-9">%.2f Nm3/kg</dd>""" output.HydrogenSpecificVolumeNTP)
     .AppendLine(sprintloc """<dt class="col-sm-3">Minimum Load Of One Line</dt> <dd class="col-sm-9">%.0f kg/h</dd>""" output.MinimumLoadOfOneLine)
     .AppendLine("</dl>")
     .AppendLine("<h3>Energy Consumption</h3>")
     .AppendLine(getTableDownloadButton "EnergyConsuptionData")
     .AppendLine("""<table id="EnergyConsuptionData" class="table table-striped"><thead><tr><th>Load</th><th>Energy consumption DC</th><th>Energy consumption AC</th>""")
     .AppendLine("<th>Energy consumption total</th><th>H2 Production</th></tr>")
     .AppendLine("""<tr><th>#</th><th>kWh/kgH2</th><th>kWh/kgH2</th>""")
     .AppendLine("<th>kWh/kgH2</th><th>kgH2/h</th></tr></thead><tbody>") |> ignore
   for ec in output.EnergyConsumptionOutput do
     sb.AppendLine("<tr>")
       .AppendLine(sprintloc "<td>%.2f%%</td>" ec.Load)
       .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionDC)
       .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionAC)
       .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionTot)
       .AppendLine(sprintf "<td>%.0f</td>" ec.NominalH2Production)
       .AppendLine("</tr>") |> ignore
   sb.AppendLine("</tbody></table>")
     .AppendLine("<h3>Consumption with degradation at partial load</h3>")
     .AppendLine(getTableDownloadButton "ConsumptionWithDegradationData")
     .AppendLine("""<table id="ConsumptionWithDegradationData" class="table table-responsive table-sm table-bordered"><thead><tr><th>Year</th>""")
     .AppendLine("""<th colspan="4" class="table-primary">DC consumption (kW)</th>""")
     .AppendLine("""<th colspan="4" class="table-secondary">TOT consumption (kW)</th>""")
     .AppendLine("""<th colspan="4" class="table-success">Specific consumption (kWh/kg)</th>""")
     .AppendLine("<th>Slope</th><th>Intercept</th><th>Min Load</th></tr>")
     .AppendLine("<tr><th>#</th>") |> ignore
   for idx in [1..3] do
     for load in output.ConsumptionOverYears.Head.Loads do
       let cssClass = 
          match idx with
          | 1 -> """ class="table-primary" """
          | 2 -> """ class="table-secondary" """
          | _ -> """ class="table-success" """
       sb.AppendLine(sprintloc2 "<th %s>%.2f%%</th>" cssClass load) |> ignore
   sb.AppendLine("<th>#</th> <th>kWh/kg</th> <th>MW</th> </tr></thead><tbody>") |> ignore
   for item in output.ConsumptionOverYears do
     sb.AppendLine(sprintf "<tr><td>%i</td>" item.Year) |> ignore
     for dc in item.DcConsum1LineWithDegradationAtPartialLoad do
       sb.AppendLine(sprintf "<td class=\"table-primary\">%.0f</td>" dc) |> ignore
     for tot in item.TotConsum1LineWithDegradationAtPartialLoad do
       sb.AppendLine(sprintf "<td class=\"table-secondary\">%.0f</td>" tot) |> ignore
     for spec in item.SpecificConsumption do
       sb.AppendLine(sprintloc "<td class=\"table-success\">%.2f</td>" spec) |> ignore
     sb.AppendLine(sprintloc3 "<td>%.5f</td><td>%.4f</td><td>%.3f</td></tr>" item.Slope item.Intercect item.MinimumLoadOfOneLine) |> ignore
   sb.AppendLine("</tbody></table>") |> ignore
   el.innerHTML <- sb.ToString()

let createCalculationYearOutput (co: CalculationYearOutput) =
   let el = document.getElementById("yearBody")
   let sb = System.Text.StringBuilder(100000)
   sb.AppendLine(sprintf "<h3>Calculation Year %i - %i</h3>" co.YearOfOperation co.Year)
     .AppendLine(getTableDownloadButton "CalculationYearData")
     .AppendLine("""<table id="CalculationYearData" class="table table-striped tableFixHead"><thead class="table-dark"><tr>""") |> ignore
   [
     """<abbr title="Date and time of the day">DT</abbr>"""
     """<abbr title="Photo Voltaic">PV</abbr>"""
     """<abbr title="Eolic production">Wind</abbr>"""
     """<abbr title="NP RES Non Programmable RES both with degradation">NPResNP</abbr>"""
     """<abbr title="P RES Programmable">PResP</abbr>"""
     """<abbr title="Battery SoC">Battery</abbr>"""
     """<abbr title="EE to electrolyser">EEToElec</abbr>"""
     """<abbr title="How many lines are working?">LW</abbr>"""
     """<abbr title="EE to each line">EEToEL</abbr>"""
     """<abbr title="Specific Consumption">SC</abbr>"""
     """<abbr title="Total H2 production">H2</abbr>"""
     """<abbr title="Hour without H2 production">HNOH2</abbr>"""
     """<abbr title="Energy to grid / RES curtailment">EEToGrid</abbr>"""
     """<abbr title="O2 Production">O2</abbr>"""
   ] |> List.iter (fun item -> sb.AppendLine(sprintf """<th>%s</th>""" item) |> ignore)
   sb.AppendLine("</tr><tr>") |> ignore
   [
     "#"
     "kWh"
     "kWh"
     "kWh"
     "kWh"
     "kWh"
     "kWh"
     "#"
     "kWh"
     "kWh/kg"
     "kg<sub>H2</sub>"
     "hr"
     "kWh"
     "kg<sub>O2</sub>"
   ] |> List.iter (fun item -> sb.AppendLine(sprintf """<th>%s</th>""" item) |> ignore)
   sb.AppendLine("</tr></thead><tbody>") |> ignore
   for cyr in co.Rows do
      sb.AppendLine(sprintf "<tr><td>%02i/%02i/%i %02i:00</td>" cyr.Day.Day cyr.Day.Month cyr.Day.Year cyr.Day.Hour)
      |> write0decCol cyr.PvOut
      |> write0decCol cyr.WindOut
      |> write0decCol cyr.NpResNonProgrammable
      |> write0decCol cyr.PResProgrammable
      |> write0decCol cyr.BatterySoC
      |> write0decCol cyr.EEToElectrolyser
      |> writeintCol cyr.LinesWorking
      |> write0decCol cyr.EEToEachLine
      |> write2decCol cyr.SpecificConsumption
      |> write0decCol cyr.TotalH2Production
      |> writeintCol cyr.HourWithoutH2Production
      |> write0decCol cyr.EnergyToGrid
      |> write0decCol cyr.O2Production
      |> ignore
      sb.AppendLine("</tr>") |> ignore
   sb.AppendLine("</tbody></table>") |> ignore
   el.innerHTML <- sb.ToString()