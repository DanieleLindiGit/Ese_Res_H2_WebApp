module App

open Browser.Dom
open DomHelpers
open Functions
open HtmlFormatters
open Validation


let GlobalCalculation () =
  let allInputs = getSystemInput ()
  let allEconomicInputs = getFinancialInputs ()
  printfn "%A" allEconomicInputs
  createBattery (BatteryWithDegradation allInputs.Battery)
  createBiomass (BiomassCalculator allInputs.BiomassGasifier)
  formatEnergyDegradationOverYears 
     (PvWithDegradationStats allInputs.PvWindHourlyData allInputs.PV)
  formatEnergyDegradationOverYears 
     (WindWithDegradationStats allInputs.PvWindHourlyData allInputs.Wind)
  createElectrolyzers (ElectrolyzersWithDegradation allInputs.Electrolyzers 20)
  createCalculationYearOutput (CalculationYear allInputs 1)
  ShowSideBar ()

  navOnClick (document.getElementById "BatteryBtn")



let StartProcess () =
  let isValid = ValidateAllInputs ()
  if isValid then
     let checkWarning = ValidateYearsOfConstruction ()
     if checkWarning then
        GlobalCalculation ()
     else 
        document.getElementById("forceStart").onclick <- fun _ -> GlobalCalculation ()

let YearlyCalculation () =
  let allInputs = getSystemInput ()
  let select = document.getElementById("yearSelection") :?> Browser.Types.HTMLSelectElement
  let year = select.selectedIndex + 1
  createCalculationYearOutput (CalculationYear allInputs year)

document.getElementById("startBtn").onclick <- fun _ -> StartProcess ()

document.getElementById("yearBtn").onclick <- fun _ -> YearlyCalculation ()

NavigationHandler ()