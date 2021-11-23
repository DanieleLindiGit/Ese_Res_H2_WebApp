module DomHelpers

open Browser.Dom
open System
open CsvParser
open Inputs
open Fable.Core


[<Emit("feather.replace({ 'aria-hidden': 'true' })")>]
let updateIcons (): unit = jsNative

let parseFloat (v:string) = float (v.Replace(".", String.Empty).Replace(",","."))

let parseFloatById elementId =
  let input = document.getElementById(elementId) :?> Browser.Types.HTMLInputElement
  parseFloat input.value

let parseOptionFloatById elementId =
  let input = document.getElementById(elementId) :?> Browser.Types.HTMLInputElement
  match input.value with
  | v when v.Length > 0 -> Some (parseFloat v)
  | _ -> None

let parseIntById elementId =
  let input = document.getElementById(elementId) :?> Browser.Types.HTMLInputElement
  int input.value

let parseFloatArrayById elementId =
   let input = document.getElementById(elementId) :?> Browser.Types.HTMLTextAreaElement
   input.value.Split([| ";" |], StringSplitOptions.RemoveEmptyEntries)
   |> Array.map (fun e-> parseFloat e)
   |> List.ofArray

let getPvWindNominalPower elementId =
   let text = document.getElementById(elementId) :?> Browser.Types.HTMLTextAreaElement
   ParseText text.value

let getPV () = {
    PV.Size = parseFloatById "PVSize"
    YearOfConstruction = parseIntById "PVYearOfConstruction"
    Degradation = parseFloatArrayById "PVDegradation"
}

let getWind () = {
    Wind.Size = parseFloatById "WindSize"
    YearOfConstruction = parseIntById "WindYearOfConstruction"
    Degradation = parseFloatArrayById "WindDegradation"
}

let getBiomassGasifier () = {
    StrawMotorSize = parseFloatById "BiomassStrawMotorSize"
    MinimumMotorLoad = parseFloatById "BiomassMinimumMotorLoad"
    BiomassLHV = parseFloatById "BiomassLHV"
    ElectricEfficiency = parseFloatById "BiomassElectricEfficiency"
    AnnualAvailability = parseFloatById "BiomassAnnualAvailability"
    Price = parseFloatById "BiomassPrice"
    Capex = parseFloatById "BiomassCapex"
    Bop = parseFloatById "BiomassBop"
}

let getBattery () = {
    Battery.YearOfConstruction = parseIntById "BatteryYearOfConstruction"
    Efficiency = parseFloatById "BatteryEfficiency"
    PowerOutput = parseFloatById "BatteryPowerOutput"
    HoursCapacity = parseIntById "BatteryHoursCapacity"
    ExtraCapacity = parseFloatById "BatteryExtraCapacity"
    SpecificPrice = parseFloatById "BatterySpecificPrice"
    OverhaulBatteries = { 
        AdditionEvery10Years = parseFloatById "OverhaulBatteriesAdditionEvery10Years"
        AdditionSpecificPrice = parseFloatById "OverhaulBatteriesAdditionSpecificPrice"
        }
    Degradation = parseFloatArrayById "BatteryDegradation"
}

let getElectrolyzers () = {
    Electrolyzers.YearOfConstruction = parseIntById "ElectrolyzersYearOfConstruction"
    Lines = parseIntById "ElectrolyzersLines"
    PowerDcConsumption = parseFloatById "ElectrolyzersPowerDcConsumption"
    NominalH2Production = parseFloatById "ElectrolyzersNominalH2Production"
    PressureProductionH2 = parseFloatById "ElectrolyzersPressureProductionH2"
    PressureNeedH2 = parseFloatById "ElectrolyzersPressureNeedH2"
    Degradation = parseFloatById "ElectrolyzersDegradation"
    WaterConsumption = parseFloatById "ElectrolyzersWaterConsumption"
    WaterDischarge = parseFloatById "ElectrolyzersWaterDischarge"
    MinimumLoadOf1Line = parseFloatById "ElectrolyzersMinimumLoadOf1Line"
    EnergyConsumption = [
        { Load = 100.0; Consumption = parseFloatById "ElectrolyzersCons100" }
        { Load = 80.0;  Consumption = parseFloatById "ElectrolyzersCons80" }
        { Load = 60.0;  Consumption = parseFloatById "ElectrolyzersCons60" }
        { Load = 40.0;  Consumption = parseFloatById "ElectrolyzersCons40" }
    ]
    PriceElectrolyzer = parseFloatById "PriceElectrolyzer"
    PriceBOP = parseFloatById "ElectrolyzerPriceBOP"
    PriceOther = parseFloatById "ElectrolyzerPriceOther"
    CoolingSystemConsumption = parseFloatById "ElectrolyzersCoolingSystemConsumption"
    GasManagementConsumption = parseFloatById "ElectrolyzersGasManagementConsumption"
    OverhaulElectrolyzer = parseFloatById "ElectrolyzerOverhaulElectrolyzer"
    H2CompressorSpecificConsumption = parseOptionFloatById "ElectrolyzersH2CompressorSpecificConsumption"
}

let getSystemInput () = {
    FirstYearOfOperationBP = parseIntById "FirstYearOfOperationBP"
    ManteinanceMonth = parseIntById "ManteinanceMonth"
    PV = getPV ()
    Wind = getWind ()
    BiomassGasifier = getBiomassGasifier ()
    Battery = getBattery ()
    Electrolyzers =  getElectrolyzers ()
    Load = {
        MinimumH2Production = parseFloatById "LoadMinimumH2Production"
    }
    Oxigen = {
        Price = parseFloatById "OxigenPrice"
        Capex = parseFloatById "OxigenCapex"
        StoragePressure = parseFloatById "OxigenStoragePressure"
        Purity = parseFloatById "OxigenPurity"
        CompressorConsumption = parseFloatById "OxigenCompressorConsumption"
    }
    PvWindHourlyData = getPvWindNominalPower "PvWindCsvData"
}

let showDiv elementId = 
   let divs = document.getElementsByClassName("section")
   [0 .. divs.length-1]
   |> List.iter (fun idx -> divs.[idx].setAttribute ("class", "section d-none"))
   document.getElementById(elementId).setAttribute ("class", "section")

let navOnClick (element: Browser.Types.Element) =
   let links = document.getElementsByClassName("nav-link")
   [0 .. links.length-1]
   |> List.iter (fun idx -> links.[idx].setAttribute("class", "nav-link"))
   element.setAttribute("class", "nav-link active")
   let target = element.getAttribute "data-target"
   showDiv target

let NavigationHandler () =
   let links = document.getElementsByClassName("nav-link")
   [0 .. links.length-1]
   |> List.iter (fun idx -> 
      let l = links.[idx] :?> Browser.Types.HTMLAnchorElement
      l.onclick <- (fun _ -> navOnClick l))

let ShowSideBar () =
   let element = document.getElementById("sidebarMenu")
   element.classList.remove("d-none")
   element.classList.add("d-md-block")