module Outputs

open System

type BatteryOutput = {
    YearOfOperation: int
    MWh: float
    MW: float
}

type BiomassGasifierOutput = {
    ThermalPower: float // MWth
    MaxBiomassConsumptionOneHour: float // t/h
    BiomassConsumption: float // kgBiomass/kWh
    MinimumLoad: float //Mwel
}

type PvWindOutput = {
    Day: System.DateTime
    PvOut: float
    WindOut: float
}

type EnergyDegradationOverYears = {
    SerieName: string
    Rows: list<int * float * float * float * float> // year total min max average
}

// Electrolizers Section
type EnergyConsumptionOutput = {
    Load: float // %
    ConsumptionDC: float // kWh/kgH2
    ConsumptionAC: float // kWh/kgH2
    ConsumptionTot: float // kWh/kgH2
    NominalH2Production: float // kgH2/h
}

type ElectrolizersTablesOutput = {
    Year: int
    YearOfOp: int
    Loads: float list
    DcConsum1LineWithDegradationAtPartialLoad: float list
    TotConsum1LineWithDegradationAtPartialLoad: float list
    SpecificConsumption: float list
    MinimumLoadOfOneLine: float
    Slope: float
    Intercect: float
}

type ElectrolyzersOutput = {
    PowerDcConsumptionTot: float // MW
    NominalH2ProductionTot: float // kgH2/h
    WaterConsumption: float // tonH20/kgH2
    WaterDischarged: float // tonH20/kgH2
    OxygenProduction: float // kgO2/kgH2
    HydrogenSpecificVolumeNTP: float // Nm3/kg
    MinimumLoadOfOneLine: float // MW
    EnergyConsumptionOutput: EnergyConsumptionOutput list
    ConsumptionOverYears: ElectrolizersTablesOutput list
}

type CalculationYearRow = {
    Day: System.DateTime
    PvOut: float
    WindOut: float
    NpResNonProgrammable: float
    MaxLoad: float
    NpResMinusMaxLoad: float
    PResProgrammable: float
    BiomassForEEProduction: float
    NpResAddPResMinusMaxLoad: float
    ToElectrolyzer: float
    NeededFromStorage: float
    PotentiallyToStorage: float
    PotentiallyFromStorage: float
    BatterySoC: float
    ChargingDischarging: float
    ActualFromStorage: float
    EEToElectrolyser: float
    PotentialEEFromGrid: float
    LinesWorking: int
    EEToEachLine: float
    AreElectrolyzersWorkingUnderMiniumLoad: int
    SpecificConsumption: float
    Module1: float
    Module2: float
    Module3: float
    TotalH2Production: float
    HourWithoutH2Production: int
    WaterConsumption: float
    WaterDischarge: float
    EnergyToGrid: float
    O2Production: float
    N2Consumption: float
    H2ToBeProduced: float
    EnergyFromGrid: float
}

let defaultCalculationYearRow = {
    CalculationYearRow.Day = new System.DateTime(1990, 1, 1)
    PvOut = 0.0
    WindOut = 0.0
    NpResNonProgrammable = 0.0
    MaxLoad = 0.0
    NpResMinusMaxLoad = 0.0
    PResProgrammable = 0.0
    BiomassForEEProduction = 0.0
    NpResAddPResMinusMaxLoad = 0.0
    ToElectrolyzer = 0.0
    NeededFromStorage = 0.0
    PotentiallyToStorage = 0.0
    PotentiallyFromStorage = 0.0
    BatterySoC = 0.0
    ChargingDischarging = 0.0
    ActualFromStorage = 0.0
    EEToElectrolyser = 0.0
    PotentialEEFromGrid = 0.0
    LinesWorking = 0
    EEToEachLine = 0.0
    AreElectrolyzersWorkingUnderMiniumLoad = 0
    SpecificConsumption = 0.0
    Module1 = 0.0
    Module2 = 0.0
    Module3 = 0.0
    TotalH2Production = 0.0
    HourWithoutH2Production = 0
    WaterConsumption = 0.0
    WaterDischarge = 0.0
    EnergyToGrid = 0.0
    O2Production = 0.0
    N2Consumption = 0.0
    H2ToBeProduced = 0.0
    EnergyFromGrid = 0.0
}

type CalculationYearOutput = {
    YearOfOperation: int
    Year: int
    Rows: CalculationYearRow list
}