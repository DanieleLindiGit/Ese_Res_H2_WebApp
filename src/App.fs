module App

open Browser.Dom
open DomHelpers
open Functions
open EconomicFunctions
open HtmlFormatters
open Validation
open SaveAndLoad


let GlobalCalculation () =
  let allInputs = getSystemInput ()
  let finInputs = getFinancialInputs ()
  createBattery (BatteryWithDegradation allInputs.Battery)
  createBiomass (BiomassCalculator allInputs.BiomassGasifier)
  formatEnergyDegradationOverYears 
     (PvWithDegradationStats allInputs.PvWindHourlyData allInputs.PV)
  formatEnergyDegradationOverYears 
     (WindWithDegradationStats allInputs.PvWindHourlyData allInputs.Wind)
  createElectrolyzers (ElectrolyzersWithDegradation allInputs.Electrolyzers finInputs.FinancialParameters.RepaimentPeriod)
  createCalculationYearOutput (CalculationYear allInputs 1)
  createBusinnesPlanTable (finalBusinessPlan allInputs finInputs)
  ShowSideBar ()

  navOnClick (document.getElementById "BusinessPlanButton")



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

document.getElementById("LoadConfigBtn").onchange <- fun evt -> LoadConfiguration evt
document.getElementById("SaveConfigBtn").onclick <- fun _ -> SaveConfig ()

NavigationHandler ()