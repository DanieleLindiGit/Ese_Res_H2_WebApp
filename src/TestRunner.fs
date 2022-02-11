module TestRunner

open Inputs
open EconomicInputs

type ProjectInput =
    { SystemInputs: SystemInputs
      FinancialInputs: FinancialInputs }


let GetProjectInputFromParameter (inp: ProjectInput) (parameter: string * float) =
    let (pname, pvalue) = parameter

    match pname with

    // +--------------------+
    // + TECHNICAL INPUTS   +
    // +--------------------+
    // General
    | "General.FirstYearOfOperationBP" ->
        Ok { inp with SystemInputs = { inp.SystemInputs with FirstYearOfOperationBP = int pvalue } }
    | "General.ManteinanceMonth" -> Ok { inp with SystemInputs = { inp.SystemInputs with ManteinanceMonth = int pvalue } }
    | "General.OEM_Costs" -> Ok { inp with FinancialInputs = { inp.FinancialInputs with OEM_Costs = pvalue } }

    // PV and Wind
    | "PV.Size" ->
        Ok { inp with SystemInputs = { inp.SystemInputs with PV = { inp.SystemInputs.PV with Size = pvalue } } }
    | "PV.YearOfConstruction" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with PV = { inp.SystemInputs.PV with YearOfConstruction = int pvalue } } }
    | "Wind.Size" ->
        Ok { inp with SystemInputs = { inp.SystemInputs with Wind = { inp.SystemInputs.Wind with Size = pvalue } } }
    | "Wind.YearOfConstruction" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Wind = { inp.SystemInputs.Wind with YearOfConstruction = int pvalue } } }

    // Biomass
    | "BiomassGasifier.StrawMotorSize" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        BiomassGasifier = { inp.SystemInputs.BiomassGasifier with StrawMotorSize = pvalue } } }
    | "BiomassGasifier.MinimumMotorLoad" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        BiomassGasifier = { inp.SystemInputs.BiomassGasifier with MinimumMotorLoad = pvalue } } }
    | "BiomassGasifier.BiomassLHV" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        BiomassGasifier = { inp.SystemInputs.BiomassGasifier with BiomassLHV = pvalue } } }
    | "BiomassGasifier.ElectricEfficiency" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        BiomassGasifier = { inp.SystemInputs.BiomassGasifier with ElectricEfficiency = pvalue } } }
    | "BiomassGasifier.AnnualAvailability" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        BiomassGasifier = { inp.SystemInputs.BiomassGasifier with AnnualAvailability = pvalue } } }
    | "BiomassGasifier.Price" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Price = pvalue } } }
    | "BiomassGasifier.Capex" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Capex = pvalue } } }
    | "BiomassGasifier.Bop" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Bop = pvalue } } }

    // Battery
    | "Battery.YearOfConstruction" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with YearOfConstruction = int pvalue } } }
    | "Battery.Efficiency" ->
        Ok
            { inp with
                SystemInputs = { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with Efficiency = pvalue } } }
    | "Battery.PowerOutput" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with PowerOutput = pvalue } } }
    | "Battery.HoursCapacity" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with HoursCapacity = int pvalue } } }
    | "Battery.ExtraCapacity" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with ExtraCapacity = pvalue } } }
    | "Battery.SpecificPrice" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with SpecificPrice = pvalue } } }
    | "OverhaulBatteries.AdditionEvery10Years" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Battery =
                            { inp.SystemInputs.Battery with
                                OverhaulBatteries =
                                    { inp.SystemInputs.Battery.OverhaulBatteries with AdditionEvery10Years = pvalue } } } }
    | "OverhaulBatteries.AdditionSpecificPrice" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Battery =
                            { inp.SystemInputs.Battery with
                                OverhaulBatteries =
                                    { inp.SystemInputs.Battery.OverhaulBatteries with AdditionSpecificPrice = pvalue } } } }

    // Electrolyzers
    | "Electrolyzers.YearOfConstruction" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with YearOfConstruction = int pvalue } } }
    | "Electrolyzers.Lines" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with Lines = int pvalue } } }
    | "Electrolyzers.PowerDcConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with PowerDcConsumption = pvalue } } }
    | "Electrolyzers.NominalH2Production" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with NominalH2Production = pvalue } } }
    | "Electrolyzers.PressureProductionH2" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with PressureProductionH2 = pvalue } } }
    | "Electrolyzers.PressureNeedH2" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with PressureNeedH2 = pvalue } } }
    | "Electrolyzers.Degradation" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with Degradation = pvalue } } }
    | "Electrolyzers.WaterConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with WaterConsumption = pvalue } } }
    | "Electrolyzers.WaterDischarge" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with WaterDischarge = pvalue } } }
    | "Electrolyzers.MinimumLoadOf1Line" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with MinimumLoadOf1Line = pvalue } } }
    | "Electrolyzers.PriceElectrolyzer" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceElectrolyzer = pvalue } } }
    | "Electrolyzers.PriceBOP" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceBOP = pvalue } } }
    | "Electrolyzers.PriceOther" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceOther = pvalue } } }
    | "Electrolyzers.CapexHydrogenCompressor" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with CapexHydrogenCompressor = pvalue } } }
    | "Electrolyzers.CoolingSystemConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with CoolingSystemConsumption = pvalue } } }
    | "Electrolyzers.GasManagementConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with GasManagementConsumption = pvalue } } }
    | "Electrolyzers.OverhaulElectrolyzer" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with OverhaulElectrolyzer = pvalue } } }
    | "Electrolyzers.H2CompressorSpecificConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers =
                            { inp.SystemInputs.Electrolyzers with H2CompressorSpecificConsumption = Some pvalue } } }
    | "Electrolyzers.OxigenCompressorConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        Electrolyzers = { inp.SystemInputs.Electrolyzers with OxigenCompressorConsumption = pvalue } } }

    // Load
    | "Load.MinimumH2Production" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Load = { inp.SystemInputs.Load with MinimumH2Production = pvalue } } }

    //OXIGEN
    | "Oxigen.Price" ->
        Ok
            { inp with SystemInputs = { inp.SystemInputs with Oxigen = { inp.SystemInputs.Oxigen with Price = pvalue } } }
    | "Oxigen.Capex" ->
        Ok
            { inp with SystemInputs = { inp.SystemInputs with Oxigen = { inp.SystemInputs.Oxigen with Capex = pvalue } } }
    | "Oxigen.StoragePressure" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Oxigen = { inp.SystemInputs.Oxigen with StoragePressure = pvalue } } }
    | "Oxigen.Purity" ->
        Ok
            { inp with
                SystemInputs = { inp.SystemInputs with Oxigen = { inp.SystemInputs.Oxigen with Purity = pvalue } } }
    | "Oxigen.CompressorConsumption" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with Oxigen = { inp.SystemInputs.Oxigen with CompressorConsumption = pvalue } } }

    // Hydrogen Storage
    | "HydrogenStorage.NumberOfHours" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        HydrogenStorage = { inp.SystemInputs.HydrogenStorage with NumberOfHours = int pvalue } } }
    | "HydrogenStorage.StoragePrice" ->
        Ok
            { inp with
                SystemInputs =
                    { inp.SystemInputs with
                        HydrogenStorage = { inp.SystemInputs.HydrogenStorage with StoragePrice = pvalue } } }


    // +--------------------+
    // +  ECONOMIC INPUTS   +
    // +--------------------+

    // Variable Costs
    | "VariableCosts.ElectricityFromPvAndWind" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        VariableCosts = { inp.FinancialInputs.VariableCosts with ElectricityFromPvAndWind = pvalue } } }
    | "VariableCosts.StrawConsumption" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        VariableCosts = { inp.FinancialInputs.VariableCosts with StrawConsumption = pvalue } } }
    | "VariableCosts.Water" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with VariableCosts = { inp.FinancialInputs.VariableCosts with Water = pvalue } } }
    | "VariableCosts.VariableForMotor" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        VariableCosts = { inp.FinancialInputs.VariableCosts with VariableForMotor = pvalue } } }
    | "VariableCosts.ElectricityToGrid" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        VariableCosts = { inp.FinancialInputs.VariableCosts with ElectricityToGrid = pvalue } } }

    // Financing
    | "Financing.Equity" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        Financing =
                            { inp.FinancialInputs.Financing with
                                Equity = pvalue
                                Debt = 100.0 - pvalue } } }

    // Financial Parameters
    | "FinancialParameters.LoanInterestRate" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters = { inp.FinancialInputs.FinancialParameters with LoanInterestRate = pvalue } } }
    | "FinancialParameters.CapitalDiscountRate" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters =
                            { inp.FinancialInputs.FinancialParameters with CapitalDiscountRate = pvalue } } }
    | "FinancialParameters.InflationRate" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters = { inp.FinancialInputs.FinancialParameters with InflationRate = pvalue } } }
    | "FinancialParameters.RepaimentPeriod" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters =
                            { inp.FinancialInputs.FinancialParameters with RepaimentPeriod = int pvalue } } }
    | "FinancialParameters.EarningTax" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters = { inp.FinancialInputs.FinancialParameters with EarningTax = pvalue } } }
    | "FinancialParameters.H2PriceEscalation" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters =
                            { inp.FinancialInputs.FinancialParameters with H2PriceEscalation = pvalue } } }
    | "FinancialParameters.AmortizationPeriod" ->
        Ok
            { inp with
                FinancialInputs =
                    { inp.FinancialInputs with
                        FinancialParameters =
                            { inp.FinancialInputs.FinancialParameters with AmortizationPeriod = int pvalue } } }


    | _ -> Error(sprintf "Parameter %s is not valid" pname)
