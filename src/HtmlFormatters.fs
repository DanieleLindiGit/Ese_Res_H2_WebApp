module HtmlFormatters

open Fable.Core
open Browser.Dom
open Outputs
open EconomicOutputs

[<Emit("new Intl.NumberFormat('it-IT', { style: 'decimal' }).format($0)")>]
let decimalFormatter (value: float) : string = jsNative

[<Emit("new Intl.NumberFormat('it-IT', { style: 'decimal', maximumFractionDigits: 0 }).format($0)")>]
let intFormatter (value: float) : string = jsNative

let sprintloc format value =
    (sprintf format value)
        .ToString()
        .Replace('.', ',')

let sprintloc2 format value1 value2 =
    (sprintf format value1 value2)
        .ToString()
        .Replace('.', ',')

let sprintloc3 format value1 value2 value3 =
    (sprintf format value1 value2 value3)
        .ToString()
        .Replace('.', ',')

let sprintloc4 format value1 value2 value3 value4 =
    (sprintf format value1 value2 value3 value4)
        .ToString()
        .Replace('.', ',')

let writeHeader (desc: string) = sprintf "<th>%s</th>" desc

let writeintCol (x: int) (s: System.Text.StringBuilder) = s.Append(sprintf "<td>%i</td>" x)

let write0decCol (x: float) (s: System.Text.StringBuilder) = s.Append(sprintf "<td>%.0f</td>" x)

let write2decCol (x: float) (s: System.Text.StringBuilder) = s.Append(sprintloc "<td>%.2f</td>" x)

let getTableDownloadButton tableId =
    sprintf
        """
     <div class="text-end">
            <a class="btn btn-secondary mb-2" href="#" onclick="download_table_as_csv('%s');">
              Download
              <img src="icons/download.svg">
            </a>
          </div> 
     """
        tableId

let createBattery (batteryOutput: BatteryOutput list) =
    let el = document.getElementById ("batteryBody")
    el.innerHTML <- ""

    for b in batteryOutput do
        let row = document.createElement ("tr") :?> Browser.Types.HTMLTableRowElement
        row.innerHTML <- sprintloc3 "<td>%i</td><td>%.2f</td><td>%.2f</td>" b.YearOfOperation b.MWh b.MW
        el.appendChild row |> ignore

let createBiomass (biomass: BiomassGasifierOutput) =
    let el = document.getElementById ("biomassBody")

    el.innerHTML <-
        sprintloc4
            """<dl class="row">
      <dt class="col-sm-3">Thermal Power</dt><dd class="col-sm-9">%.0f MWth</dd>
      <dt class="col-sm-3">Max Biomass consumption 1h</dt><dd class="col-sm-9">%.1f t/h</dd>
      <dt class="col-sm-3">Biomass Consumption</dt><dd class="col-sm-9">%.1f kgBiomass/kWh</dd>
      <dt class="col-sm-3">Minimum Load</dt><dd class="col-sm-9">%.1f MW<sub>el</sub></dd>
      </dl>"""
            biomass.ThermalPower
            biomass.MaxBiomassConsumptionOneHour
            biomass.BiomassConsumption
            biomass.MinimumLoad

let formatEnergyDegradationOverYears (ed: EnergyDegradationOverYears) =
    let el = document.getElementById (ed.SerieName + "Div")
    let sb = System.Text.StringBuilder(10000)
    let tableId = ed.SerieName + "Data"

    let image =
        match ed.SerieName with
        | "PV" -> """ <img title="img" src="icons/sun.svg"> """
        | _ -> """ <img title="wind" src="icons/wind.svg"> """

    sb.AppendLine(sprintf "<h3>%s %s with Degradation</h3><hr>" image ed.SerieName)
    |> ignore

    sb.AppendLine(getTableDownloadButton tableId)
    |> ignore

    sb.AppendLine(
        "<table id=\""
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
            <tbody>"""
    )
    |> ignore

    for (year, total, min, max, avg) in ed.Rows do
        sb.AppendLine(
            sprintf
                "<tr><td>%i</td><td>%s</td><td>%.0f</td><td>%.0f</td><td>%.0f</td></tr>"
                year
                (intFormatter total)
                min
                max
                avg
        )
        |> ignore

    sb.AppendLine "</tbody></table>" |> ignore
    el.innerHTML <- sb.ToString()

let createElectrolyzers (output: ElectrolyzersOutput) =
    let el = document.getElementById ("ElectrolyzersDiv")
    let sb = System.Text.StringBuilder(10000)

    sb
        .AppendLine(""" <h3> <img title="img" src="icons/droplet.svg"> Electrolyzers Data Output</h3><hr> """)
        .AppendLine("""<dl class="row">""")
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Nominal H2 Production Total</dt> <dd class="col-sm-9">%.0f kgH2/h</dd>"""
                output.NominalH2ProductionTot
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">WaterConsumption Total</dt> <dd class="col-sm-9">%.4f tonH20/kgH2</dd>"""
                output.WaterConsumption
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Water Discharged Total</dt> <dd class="col-sm-9">%.3f tonH20/kgH2</dd>"""
                output.WaterDischarged
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Oxygen Production Total</dt> <dd class="col-sm-9">%.0f kgO2/kgH2</dd>"""
                output.OxygenProduction
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Hydrogen Specific Volume NTP</dt> <dd class="col-sm-9">%.2f Nm3/kg</dd>"""
                output.HydrogenSpecificVolumeNTP
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Minimum Load Of One Line</dt> <dd class="col-sm-9">%.0f kg/h</dd>"""
                output.MinimumLoadOfOneLine
        )
        .AppendLine("</dl>")
        .AppendLine("<h3>Energy Consumption</h3>")
        .AppendLine(getTableDownloadButton "EnergyConsuptionData")
        .AppendLine(
            """<table id="EnergyConsuptionData" class="table table-striped"><thead><tr><th>Load</th><th>Energy consumption DC</th><th>Energy consumption AC</th>"""
        )
        .AppendLine("<th>Energy consumption total</th><th>H2 Production</th></tr>")
        .AppendLine("""<tr><th>#</th><th>kWh/kgH2</th><th>kWh/kgH2</th>""")
        .AppendLine("<th>kWh/kgH2</th><th>kgH2/h</th></tr></thead><tbody>")
    |> ignore

    for ec in output.EnergyConsumptionOutput do
        sb
            .AppendLine("<tr>")
            .AppendLine(sprintloc "<td>%.2f%%</td>" ec.Load)
            .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionDC)
            .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionAC)
            .AppendLine(sprintloc "<td>%.2f</td>" ec.ConsumptionTot)
            .AppendLine(sprintf "<td>%.0f</td>" ec.NominalH2Production)
            .AppendLine("</tr>")
        |> ignore

    sb
        .AppendLine("</tbody></table>")
        .AppendLine("<h3>Consumption with degradation at partial load: one line</h3>")
        .AppendLine(getTableDownloadButton "ConsumptionWithDegradationData")
        .AppendLine(
            """<table id="ConsumptionWithDegradationData" class="table table-responsive table-sm table-bordered"><thead><tr><th>Year</th>"""
        )
        .AppendLine("""<th colspan="4" class="table-primary">DC consumption (kW)</th>""")
        .AppendLine("""<th colspan="4" class="table-secondary">TOT consumption (kW)</th>""")
        .AppendLine("""<th colspan="4" class="table-success">Specific consumption (kWh/kg)</th>""")
        .AppendLine("<th>Slope</th><th>Intercept</th><th>Min Load</th></tr>")
        .AppendLine("<tr><th>#</th>")
    |> ignore

    for idx in [ 1..3 ] do
        for load in output.ConsumptionOverYears.Head.Loads do
            let cssClass =
                match idx with
                | 1 -> """ class="table-primary" """
                | 2 -> """ class="table-secondary" """
                | _ -> """ class="table-success" """

            sb.AppendLine(sprintloc2 "<th %s>%.2f%%</th>" cssClass load)
            |> ignore

    sb.AppendLine("<th>#</th> <th>kWh/kg</th> <th>MW</th> </tr></thead><tbody>")
    |> ignore

    for item in output.ConsumptionOverYears do
        sb.AppendLine(sprintf "<tr><td>%i</td>" item.Year)
        |> ignore

        for dc in item.DcConsum1LineWithDegradationAtPartialLoad do
            sb.AppendLine(sprintf "<td class=\"table-primary\">%.0f</td>" dc)
            |> ignore

        for tot in item.TotConsum1LineWithDegradationAtPartialLoad do
            sb.AppendLine(sprintf "<td class=\"table-secondary\">%.0f</td>" tot)
            |> ignore

        for spec in item.SpecificConsumption do
            sb.AppendLine(sprintloc "<td class=\"table-success\">%.2f</td>" spec)
            |> ignore

        sb.AppendLine(
            sprintloc3
                "<td>%.5f</td><td>%.4f</td><td>%.3f</td></tr>"
                item.Slope
                item.Intercect
                item.MinimumLoadOfOneLine
        )
        |> ignore

    sb.AppendLine("</tbody></table>") |> ignore
    el.innerHTML <- sb.ToString()

let createCalculationYearOutput (co: CalculationYearOutput) =
    let maxRows = 50
    let el = document.getElementById ("yearBody")
    let sb = System.Text.StringBuilder(100000)

    sb
        .AppendLine(
            sprintf
                """ <h3> <img title="img" src="icons/briefcase.svg"> Calculation Year %i - %i</h3> """
                co.YearOfOperation
                co.Year
        )
        .AppendLine(
            sprintf
                """
      <div class="alert alert-info" role="alert">
         This is a preview of first %i rows. Please download the file to get all data.
      </div>
      """
                maxRows
        )
        .AppendLine(getTableDownloadButton "CalculationYearData")
        .AppendLine(
            """<table id="CalculationYearData" class="table table-striped tableFixHead"><thead class="table-dark"><tr>"""
        )
    |> ignore

    [ """<abbr title="Date and time of the day">DT</abbr>"""
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
      """<abbr title="O2 Production">O2</abbr>""" ]
    |> List.iter (fun item ->
        sb.AppendLine(sprintf """<th>%s</th>""" item)
        |> ignore)

    sb.AppendLine("</tr><tr>") |> ignore

    [ "#"
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
      "kg<sub>O2</sub>" ]
    |> List.iter (fun item ->
        sb.AppendLine(sprintf """<th>%s</th>""" item)
        |> ignore)

    sb.AppendLine("</tr></thead><tbody>") |> ignore
    let indexData = co.Rows |> List.indexed

    for dr in indexData do
        let idx, cyr = dr

        let cn =
            match idx with
            | i when i > maxRows -> """ class="d-none" """
            | _ -> ""

        sb.AppendLine(
            sprintf "<tr%s><td>%02i/%02i/%i %02i:00</td>" cn cyr.Day.Day cyr.Day.Month cyr.Day.Year cyr.Day.Hour
        )
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


type BusinessPlanColumn =
    { TotalYear: string
      OperationalYear: string
      Year: string

      CyEquity: string
      CyDebt: string
      CyIDC: string

      Amortization: string

      HydrogenProduction: string // kgH2 SUM OF TotalH2Production
      HydrogenRevenues: string // €
      OxigenProduction: string // kg SUM OF O2Production
      OxigenRevenues: string // €
      ExtraEEtoGrid: string // MWh SUM OF EnergyToGrid / 1000
      ExtraEEtoGridRevenues: string // €
      TotalRevenues: string

      FixedCosts: string // €
      ToElectrolyzer: string // SUM OF ToElectrolyzer
      PResProgrammable: string // SUM OF PResProgrammable
      BatteryCharging: string // SUM OF Charging
      ElectricityCost: string // € calculated
      WaterConsumption: string // SUM OF WaterConsumption
      WaterCost: string // €
      BiomassForEEProduction: string // SUM OF BiomassForEEProduction
      BiomassCost: string // €
      TotalCosts: string

      EBIT: string
      DebtReimbursement: string
      DebtInterest: string
      EBT: string
      TAX: string
      EAT: string
      FCF: string }

let getBpColumnFromConstructionYear (cy: ConstructionYear) =
    { BusinessPlanColumn.TotalYear = sprintf "%i" cy.TotalYear
      OperationalYear = ""
      Year = sprintf "%i" cy.Year

      CyEquity = intFormatter cy.CyEquity
      CyDebt = intFormatter cy.CyDebt
      CyIDC = intFormatter cy.CyIDC

      Amortization = ""

      HydrogenProduction = ""
      HydrogenRevenues = ""
      OxigenProduction = ""
      OxigenRevenues = ""
      ExtraEEtoGrid = ""
      ExtraEEtoGridRevenues = ""
      TotalRevenues = ""

      FixedCosts = ""
      ToElectrolyzer = ""
      PResProgrammable = ""
      BatteryCharging = ""
      ElectricityCost = ""
      WaterConsumption = ""
      WaterCost = ""
      BiomassForEEProduction = ""
      BiomassCost = ""
      TotalCosts = ""

      EBIT = ""
      DebtReimbursement = ""
      DebtInterest = ""
      EBT = ""
      TAX = ""
      EAT = ""
      FCF = "" }

let getBpColumnFromYearAnalysis (ya: YearAnalysis) =
    { BusinessPlanColumn.TotalYear = sprintf "%i" ya.TotalYear
      OperationalYear = sprintf "%i" ya.OperationalYear
      Year = sprintf "%i" ya.Year

      CyEquity = ""
      CyDebt = ""
      CyIDC = ""

      Amortization = intFormatter ya.Amortization

      HydrogenProduction = intFormatter ya.HydrogenProduction
      HydrogenRevenues = intFormatter ya.HydrogenRevenues
      OxigenProduction = intFormatter ya.OxigenProduction
      OxigenRevenues = intFormatter ya.OxigenRevenues
      ExtraEEtoGrid = intFormatter ya.ExtraEEtoGrid
      ExtraEEtoGridRevenues = intFormatter ya.ExtraEEtoGridRevenues
      TotalRevenues = intFormatter ya.TotalRevenues

      FixedCosts = intFormatter ya.FixedCosts
      ToElectrolyzer = intFormatter ya.ToElectrolyzer
      PResProgrammable = intFormatter ya.PResProgrammable
      BatteryCharging = intFormatter ya.BatteryCharging
      ElectricityCost = intFormatter ya.ElectricityCost
      WaterConsumption = intFormatter ya.WaterConsumption
      WaterCost = intFormatter ya.WaterCost
      BiomassForEEProduction = intFormatter ya.BiomassForEEProduction
      BiomassCost = intFormatter ya.BiomassCost
      TotalCosts = intFormatter ya.TotalCosts

      EBIT = intFormatter ya.EBIT
      DebtReimbursement = intFormatter ya.DebtReimbursement
      DebtInterest = intFormatter ya.DebtInterest
      EBT = intFormatter ya.EBT
      TAX = intFormatter ya.Tax
      EAT = intFormatter ya.EAT
      FCF = intFormatter ya.FCF }

type ClassRow =
    | RowNotset
    | RowActive
    | RowSuccess
    | RowWarning
    | RowSecondary
    | RowInfo

let createBusinnesPlanTable (bpo: BusinessPlanOutput) =
    let col1 =
        bpo.ConstructionYears
        |> List.map (fun v -> getBpColumnFromConstructionYear v)

    let col2 =
        bpo.YearsAnalysis
        |> List.map (fun v -> getBpColumnFromYearAnalysis v)

    let columns = col1 @ col2

    let joinColumn (firstCol: string) (values: string list) (classRow: ClassRow) =
        let trCn =
            match classRow with
            | RowNotset -> ""
            | RowActive -> """ class="table-active" """
            | RowSuccess -> """ class="table-success" """
            | RowWarning -> """ class="table-warning" """
            | RowSecondary -> """ class="fw-light fst-italic" """
            | RowInfo -> """ class="table-info" """

        let temp1 =
            sprintf "<tr %s><th class=\"first-bp-col\">%s</th><td class=\"text-end\">" trCn firstCol

        let temp2 =
            values
            |> String.concat "</td><td class=\"text-end\">"

        let temp3 = "</td></tr>"
        temp1 + temp2 + temp3

    let el = document.getElementById ("BusinessPlanTable")
    let sb = System.Text.StringBuilder()

    sb
        .AppendLine(""" <h3> <img title="img" src="icons/briefcase.svg"> Business Plan </h3><hr> """)
        .AppendLine("<h4>Cost Analysis</h4>")
        .AppendLine("""<dl class="row">""")
        .AppendLine(sprintloc """<dt class="col-sm-2">LCOH</dt> <dd class="col-sm-2">%.4f €/kg</dd>""" bpo.LCOH)
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Net Present Value</dt> <dd class="col-sm-2">%.0f €(Thousands)</dd>"""
                bpo.BpNPV
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Internal Rate of Return</dt> <dd class="col-sm-2">%.0f %%</dd>"""
                bpo.BpIRR
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Total Debt</dt> <dd class="col-sm-2">%s €(Thousands)</dd>"""
                (intFormatter bpo.TotalDebt)
        )
        .AppendLine("</dl>")
        .AppendLine("<h4>Technical and econominal parameters</h4>")
        .AppendLine("""<dl class="row">""")
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Electrolyzer - Nominal Stack Power</dt> <dd class="col-sm-2">%.2f MW</dd>"""
                bpo.BusinessPlanInput.TechAndEconomicalParameters.ElectrolizerNominalStackPower
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-3">Batteries - Energy Capacity</dt> <dd class="col-sm-2">%.2f MWh</dd>"""
                bpo.BusinessPlanInput.TechAndEconomicalParameters.BatteriesEnergyCapacity
        )
        .AppendLine("</dl>")
        .AppendLine("<h4>Capex Cost</h4>")
        .AppendLine("""<dl class="row">""")
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Electrolyzer Equipment</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.ElectrolizerEquipment)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Batteries</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.Batteries)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Straw Gasifier</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.StrawGasifier)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Hydrogen Storage 24h</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.HydrogenStorage)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Overhaul Batteries</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.OverhaulBatteriesCost)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Overhaul Electrolyzer</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.OverhaulElectrolizer)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Hydrogen Compressor</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.HydrogenCompressor)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Oxygen Compressor</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.OxigenCompressor)
        )
        .AppendLine(
            sprintloc
                """<dt class="col-sm-2">Total</dt> <dd class="col-sm-2 text-end">%s €</dd>"""
                (intFormatter bpo.BusinessPlanInput.CapexCost.Total)
        )
        .AppendLine("</dl>")
        .AppendLine("<h4>Cash Flow</h4>")
        .AppendLine(getTableDownloadButton "BusinessPlanData")
        .AppendLine("""<table id="BusinessPlanData" class="table table-bordered table-sm"><tbody>""")
        .AppendLine(joinColumn "Total Year" (columns |> List.map (fun v -> v.TotalYear)) RowNotset)
        .AppendLine(joinColumn "Operational Year" (columns |> List.map (fun v -> v.OperationalYear)) RowNotset)
        .AppendLine(joinColumn "Year" (columns |> List.map (fun v -> v.Year)) RowActive)
        .AppendLine(joinColumn "Equity" (columns |> List.map (fun v -> v.CyEquity)) RowNotset)
        .AppendLine(joinColumn "Debt" (columns |> List.map (fun v -> v.CyDebt)) RowNotset)
        .AppendLine(joinColumn "IDC" (columns |> List.map (fun v -> v.CyIDC)) RowNotset)
        .AppendLine(joinColumn "Amortization" (columns |> List.map (fun v -> v.Amortization)) RowActive)
        .AppendLine(
            joinColumn
                "Hydrogen Production"
                (columns
                 |> List.map (fun v -> v.HydrogenProduction))
                RowSecondary
        )
        .AppendLine(joinColumn "Hydrogen Revenues" (columns |> List.map (fun v -> v.HydrogenRevenues)) RowSecondary)
        .AppendLine(joinColumn "Oxigen Production" (columns |> List.map (fun v -> v.OxigenProduction)) RowSecondary)
        .AppendLine(joinColumn "Oxigen Revenues" (columns |> List.map (fun v -> v.OxigenRevenues)) RowSecondary)
        .AppendLine(joinColumn "Extra EE to Grid" (columns |> List.map (fun v -> v.ExtraEEtoGrid)) RowSecondary)
        .AppendLine(
            joinColumn
                "Extra EE Revenues"
                (columns
                 |> List.map (fun v -> v.ExtraEEtoGridRevenues))
                RowSecondary
        )
        .AppendLine(joinColumn "Total Revenues" (columns |> List.map (fun v -> v.TotalRevenues)) RowSuccess)
        .AppendLine(joinColumn "Fixed Costs" (columns |> List.map (fun v -> v.FixedCosts)) RowSecondary)
        .AppendLine(joinColumn "To Electrolyzer" (columns |> List.map (fun v -> v.ToElectrolyzer)) RowSecondary)
        .AppendLine(joinColumn "PRes Programmable" (columns |> List.map (fun v -> v.PResProgrammable)) RowSecondary)
        .AppendLine(joinColumn "Battery Charging" (columns |> List.map (fun v -> v.BatteryCharging)) RowSecondary)
        .AppendLine(joinColumn "Electricity Cost" (columns |> List.map (fun v -> v.ElectricityCost)) RowSecondary)
        .AppendLine(joinColumn "Water Consumption" (columns |> List.map (fun v -> v.WaterConsumption)) RowSecondary)
        .AppendLine(joinColumn "Water Cost" (columns |> List.map (fun v -> v.WaterCost)) RowSecondary)
        .AppendLine(
            joinColumn
                "Biomass For EE Production"
                (columns
                 |> List.map (fun v -> v.BiomassForEEProduction))
                RowSecondary
        )
        .AppendLine(joinColumn "Biomass Cost" (columns |> List.map (fun v -> v.BiomassCost)) RowSecondary)
        .AppendLine(joinColumn "Total Costs" (columns |> List.map (fun v -> v.TotalCosts)) RowWarning)
        .AppendLine(joinColumn "EBIT" (columns |> List.map (fun v -> v.EBIT)) RowNotset)
        .AppendLine(joinColumn "Debt Reimbursement" (columns |> List.map (fun v -> v.DebtReimbursement)) RowNotset)
        .AppendLine(joinColumn "Debt Interest" (columns |> List.map (fun v -> v.DebtInterest)) RowNotset)
        .AppendLine(joinColumn "EBT" (columns |> List.map (fun v -> v.EBT)) RowNotset)
        .AppendLine(joinColumn "TAX" (columns |> List.map (fun v -> v.TAX)) RowNotset)
        .AppendLine(joinColumn "EAT" (columns |> List.map (fun v -> v.EAT)) RowNotset)
        .AppendLine(joinColumn "FCF" (columns |> List.map (fun v -> v.FCF)) RowInfo)
        .AppendLine("</tbody></table>")
    |> ignore

    el.innerHTML <- sb.ToString()
