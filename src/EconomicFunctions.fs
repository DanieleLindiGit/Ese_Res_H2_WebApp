module EconomicFunctions

open Inputs
open Outputs
open EconomicInputs
open EconomicOutputs

let getTechAndEconomicalParameters 
   (inp: SystemInputs) 
   (elec: ElectrolyzersOutput) =
   {
       ElectrolizerNominalStackPower = 
          inp.Electrolyzers.EnergyConsumption.Head.Consumption *
          elec.HydrogenSpecificVolumeNTP
 
       BatteriesEnergyCapacity = 
          float inp.Battery.HoursCapacity *
          inp.Battery.PowerOutput *
          (1.0 + inp.Battery.ExtraCapacity)
   }

let getCapexCost
   (inp: SystemInputs) 
   (elec: ElectrolyzersOutput)
   (tec: TechAndEconomicalParameters) =
      {
       //F14*1000*(Input!B95+Input!B96+Input!B97)
       CapexCost.ElectrolizerEquipment = 
          tec.ElectrolizerNominalStackPower * 1000.0 *
          (inp.Electrolyzers.PriceElectrolyzer + inp.Electrolyzers.PriceBOP + inp.Electrolyzers.PriceOther)
       
       //F15*1000*Input!B46
       Batteries = tec.BatteriesEnergyCapacity * 1000.0 * inp.Battery.SpecificPrice

       //Input!$B$35*Input!$B$27*(1+Input!B36)
       StrawGasifier = 
          inp.BiomassGasifier.Capex *
          inp.BiomassGasifier.StrawMotorSize *
          (1.0 + inp.BiomassGasifier.Bop)

       //Input!B76*Input!B74*Input!B137*Input!B136
       HydrogenStorage = 
          inp.Electrolyzers.NominalH2Production *
          float inp.Electrolyzers.Lines *
          float 24 * // TODO: Hydrogen storage -> Number of hours
          500.0 // TODO: Hydrogen storage -> Storage price

        //Input!B45*Input!B44
       OverhaulBatteriesCost = 
          inp.Battery.OverhaulBatteries.AdditionEvery10Years *
          inp.Battery.OverhaulBatteries.AdditionSpecificPrice
        
        // Input!B98*F14
       OverhaulElectrolizer = inp.Electrolyzers.OverhaulElectrolyzer * tec.ElectrolizerNominalStackPower

       //Input!B81*Electrolyzr_with_degradation!B2
       HydrogenCompressor = 
          0.0 * //TODO: Capex hydrogen compressor
          elec.NominalH2ProductionTot

       //Input!B126*Electrolyzr_with_degradation!B6
       OxigenCompressor = 
          0.0 * // TODO: Capex per produrre ossigeno (compressore) 
          elec.OxygenProduction * elec.NominalH2ProductionTot

       Total = 0.0

      } 