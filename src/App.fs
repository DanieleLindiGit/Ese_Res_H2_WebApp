module App

open Browser.Dom
open DomHelpers
open Functions
open HtmlFormatters

let StartProcess () =
   let allInputs = getSystemInput ()
   createBattery (BatteryWithDegradation allInputs.Battery)
   createBiomass (BiomassCalculator allInputs.BiomassGasifier)
   formatEnergyDegradationOverYears 
      (PvWithDegradation allInputs.PvWindHourlyData allInputs.PV)
   formatEnergyDegradationOverYears 
      (WindWithDegradation allInputs.PvWindHourlyData allInputs.Wind)
   createElectrolyzers (ElectrolyzersWithDegradation allInputs.Electrolyzers)
   createCalculationYearOutput (CalculationYear allInputs 1)
   updateIcons ()
   ShowSideBar ()

   navOnClick (document.getElementById "BatteryBtn")

let YearlyCalculation () =
  let allInputs = getSystemInput ()
  let select = document.getElementById("yearSelection") :?> Browser.Types.HTMLSelectElement
  let year = select.selectedIndex + 1
  createCalculationYearOutput (CalculationYear allInputs year)

document.getElementById("startBtn").onclick <- fun _ -> StartProcess ()

document.getElementById("yearBtn").onclick <- fun _ -> YearlyCalculation ()

NavigationHandler ()