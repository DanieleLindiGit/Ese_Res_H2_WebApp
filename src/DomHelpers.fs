module DomHelpers

open Browser.Dom
open System
open CsvParser
open Inputs
open EconomicInputs
open Fable.Core

// HELPERS METHODS START
let parseFloat (v: string) =
    float (v.Replace(".", String.Empty).Replace(",", "."))

let parseFloatById elementId =
    let input = document.getElementById (elementId) :?> Browser.Types.HTMLInputElement
    parseFloat input.value

let parseOptionFloatById elementId =
    let input = document.getElementById (elementId) :?> Browser.Types.HTMLInputElement

    match input.value with
    | v when v.Length > 0 -> Some(parseFloat v)
    | _ -> None

let parseIntById elementId =
    let input = document.getElementById (elementId) :?> Browser.Types.HTMLInputElement
    int input.value

let parseFloatArrayById elementId =
    let input =
        document.getElementById (elementId) :?> Browser.Types.HTMLTextAreaElement

    input.value.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseFloat
    |> List.ofArray

// HELPERS METHODS END

// SYSTEM INPUTS START
let getPvWindNominalPower elementId =
    let text = document.getElementById (elementId) :?> Browser.Types.HTMLTextAreaElement
    ParseText text.value

let getPV () =
    { PV.Size = parseFloatById "PV.Size"
      YearOfConstruction = parseIntById "PV.YearOfConstruction"
      Degradation = parseFloatArrayById "PV.Degradation" }

let getWind () =
    { Wind.Size = parseFloatById "Wind.Size"
      YearOfConstruction = parseIntById "Wind.YearOfConstruction"
      Degradation = parseFloatArrayById "Wind.Degradation" }

let getBiomassGasifier () =
    { StrawMotorSize = parseFloatById "BiomassGasifier.StrawMotorSize"
      MinimumMotorLoad = parseFloatById "BiomassGasifier.MinimumMotorLoad"
      BiomassLHV = parseFloatById "BiomassGasifier.BiomassLHV"
      ElectricEfficiency = parseFloatById "BiomassGasifier.ElectricEfficiency"
      AnnualAvailability = parseFloatById "BiomassGasifier.AnnualAvailability"
      Price = parseFloatById "BiomassGasifier.Price"
      Capex = parseFloatById "BiomassGasifier.Capex"
      Bop = parseFloatById "BiomassGasifier.Bop" }

let getBattery () =
    { Battery.YearOfConstruction = parseIntById "Battery.YearOfConstruction"
      Efficiency = parseFloatById "Battery.Efficiency"
      PowerOutput = parseFloatById "Battery.PowerOutput"
      HoursCapacity = parseIntById "Battery.HoursCapacity"
      ExtraCapacity = parseFloatById "Battery.ExtraCapacity"
      SpecificPrice = parseFloatById "Battery.SpecificPrice"
      OverhaulBatteries =
        { AdditionEvery10Years = parseFloatById "Battery.OverhaulBatteries.AdditionEvery10Years"
          AdditionSpecificPrice = parseFloatById "Battery.OverhaulBatteries.AdditionSpecificPrice" }
      Degradation = parseFloatArrayById "Battery.Degradation" }

let getElectrolyzers () =
    { Electrolyzers.YearOfConstruction = parseIntById "Electrolyzers.YearOfConstruction"
      Lines = parseIntById "Electrolyzers.Lines"
      NominalH2Production = parseFloatById "Electrolyzers.NominalH2Production"
      PressureProductionH2 = parseFloatById "Electrolyzers.PressureProductionH2"
      PressureNeedH2 = parseFloatById "Electrolyzers.PressureNeedH2"
      Degradation = parseFloatById "Electrolyzers.Degradation"
      WaterConsumption = parseFloatById "Electrolyzers.WaterConsumption"
      WaterDischarge = parseFloatById "Electrolyzers.WaterDischarge"
      MinimumLoadOf1Line = parseFloatById "Electrolyzers.MinimumLoadOf1Line"
      EnergyConsumption =
        [ { Load = 100.0
            Consumption = parseFloatById "Electrolyzers.EnergyConsumption.Load100" }
          { Load = 80.0
            Consumption = parseFloatById "Electrolyzers.EnergyConsumption.Load80" }
          { Load = 60.0
            Consumption = parseFloatById "Electrolyzers.EnergyConsumption.Load60" }
          { Load = 40.0
            Consumption = parseFloatById "Electrolyzers.EnergyConsumption.Load40" } ]
      PriceElectrolyzer = parseFloatById "Electrolyzers.PriceElectrolyzer"
      PriceBOP = parseFloatById "Electrolyzers.PriceBOP"
      PriceOther = parseFloatById "Electrolyzers.PriceOther"
      CapexHydrogenCompressor = parseFloatById "Electrolyzers.CapexHydrogenCompressor"
      CoolingSystemConsumption = parseFloatById "Electrolyzers.CoolingSystemConsumption"
      GasManagementConsumption = parseFloatById "Electrolyzers.GasManagementConsumption"
      OverhaulElectrolyzer = parseFloatById "Electrolyzers.OverhaulElectrolyzer"
      H2CompressorSpecificConsumption = parseOptionFloatById "Electrolyzers.H2CompressorSpecificConsumption"
      OxigenCompressorConsumption = parseFloatById "Oxigen.CompressorConsumption" }

let getSystemInput () =
    { FirstYearOfOperationBP = parseIntById "General.FirstYearOfOperationBP"
      ManteinanceMonth = parseIntById "General.ManteinanceMonth"
      PV = getPV ()
      Wind = getWind ()
      BiomassGasifier = getBiomassGasifier ()
      Battery = getBattery ()
      Electrolyzers = getElectrolyzers ()
      Load = { MinimumH2Production = parseFloatById "Load.MinimumH2Production" }
      Oxigen =
        { Price = parseFloatById "Oxigen.Price"
          Capex = parseFloatById "Oxigen.Capex"
          StoragePressure = parseFloatById "Oxigen.StoragePressure"
          Purity = parseFloatById "Oxigen.Purity"
          CompressorConsumption = parseFloatById "Oxigen.CompressorConsumption" }
      HydrogenStorage =
        { NumberOfHours = parseIntById "HydrogenStorage.NumberOfHours"
          StoragePrice = parseFloatById "HydrogenStorage.StoragePrice" }
      PvWindHourlyData = getPvWindNominalPower "PvWindCsvData" }
// SYSTEM INPUTS END

// ECONOMIC INPUTS REGION START
let getVariableCosts () =
    { VariableCosts.ElectricityFromPvAndWind = parseFloatById "VariableCosts.ElectricityFromPvAndWind"
      StrawConsumption = parseFloatById "VariableCosts.StrawConsumption"
      Water = parseFloatById "VariableCosts.Water"
      VariableForMotor = parseFloatById "VariableCosts.VariableForMotor"
      ElectricityToGrid = parseFloatById "VariableCosts.ElectricityToGrid" }

let getFinancing () =
    let equity = parseFloatById "Financing.Equity"

    { Financing.Equity = equity
      Debt = 100.0 - equity }

let getFinancialParameters () =
    { LoanInterestRate = parseFloatById "FinancialParameters.LoanInterestRate"
      CapitalDiscountRate = parseFloatById "FinancialParameters.CapitalDiscountRate"
      InflationRate = parseFloatById "FinancialParameters.InflationRate"
      RepaimentPeriod = parseIntById "FinancialParameters.RepaimentPeriod"
      EarningTax = parseFloatById "FinancialParameters.EarningTax"
      H2PriceEscalation = parseFloatById "FinancialParameters.H2PriceEscalation"
      AmortizationPeriod = parseIntById "FinancialParameters.AmortizationPeriod" }

let getFinancialInputs () =
    { FinancialInputs.OEM_Costs = parseFloatById "General.OEM_Costs"
      VariableCosts = getVariableCosts ()
      Financing = getFinancing ()
      FinancialParameters = getFinancialParameters ()
      InitialInvestmentBreakdown = parseFloatArrayById "General.InitialInvestmentBreakdown" }

// ECONOMIC INPUTS REGION END

let showDiv elementId =
    let divs = document.getElementsByClassName ("section")

    [ 0 .. divs.length - 1 ]
    |> List.iter (fun idx ->
        divs.[idx]
            .setAttribute ("class", "section d-none"))

    document
        .getElementById(elementId)
        .setAttribute ("class", "section")

let navOnClick (element: Browser.Types.Element) =
    let links = document.getElementsByClassName ("nav-link")

    [ 0 .. links.length - 1 ]
    |> List.iter (fun idx -> links.[idx].setAttribute ("class", "nav-link"))

    element.setAttribute ("class", "nav-link active")
    let target = element.getAttribute "data-target"
    showDiv target

let NavigationHandler () =
    let links = document.getElementsByClassName ("nav-link")

    [ 0 .. links.length - 1 ]
    |> List.iter (fun idx ->
        let l = links.[idx] :?> Browser.Types.HTMLAnchorElement
        l.onclick <- (fun _ -> navOnClick l))

let ShowSideBar () =
    let element = document.getElementById ("sidebarMenu")
    element.classList.remove ("d-none")
    element.classList.add ("d-md-block")
