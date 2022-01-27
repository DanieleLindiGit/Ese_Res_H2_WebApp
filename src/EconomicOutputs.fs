module EconomicOutputs

type TechAndEconomicalParameters = {
    ElectrolizerNominalStackPower: float // MW
    BatteriesEnergyCapacity: float // MWh
}

type CapexCost = {
    ElectrolizerEquipment: float // €
    Batteries: float // €
    StrawGasifier: float // €
    HydrogenStorage: float // €
    OverhaulBatteries: float // €
    OverhaulElectrolizer: float // €
    HydrogenCompressor: float // €
    OxigenCompressor: float // €
    Total: float // €
}

type YearAnalysis = {
    Amortization: float // €

    // REVENUES
    HydrogenProduction: float // kgH2 SUM OF TotalH2Production
    HydrogenRevenues: float // €
    OxigenProduction: float // kg SUM OF O2Production
    OxigenRevenues: float // €
    ExtraEEtoGrid: float // MWh SUM OF EnergyToGrid / 1000
    ExtraEEtoGridRevenues: float // €
    TotalRevenues: float // €

    // COSTS
    FixedCosts: float // €
    ToElectrolyzer: float // SUM OF ToElectrolyzer
    PResProgrammable: float // SUM OF PResProgrammable
    BatteryCharging: float // SUM OF Charging
    ElectricityCost: float // € calculated
    WaterConsumption: float // SUM OF WaterConsumption
    WaterCost: float // €
    BiomassForEEProduction: float // SUM OF BiomassForEEProduction
    BiomassCost: float // € 
    TotalCost: float // €

    //Pure Financial Calculation
    Ebit: float
    DebtReimbursement: float
    DebtInterest: float
    Ebt: float
    Tax: float
    Eat: float
    Fcf: float
}

