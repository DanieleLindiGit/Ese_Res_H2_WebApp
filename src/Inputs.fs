module Inputs

// Fotovoltaico
type PV = {
    Size: float // MW
    YearOfConstruction: int
    Degradation: float list
}

let defaultPV = {
    PV.Size = 80.0
    YearOfConstruction = 2024
    Degradation = [ 100.00; 99.63; 99.26; 98.89; 98.53; 98.16; 
                   97.79; 97.42; 97.05; 96.68; 96.32; 95.95; 
                   95.58; 95.21; 94.84; 94.47; 94.11; 93.74; 
                   93.37; 93.00 ]
}

// Eolico
type Wind = {
    Size: float // MW
    YearOfConstruction: int
    Degradation: float list
}

let defaultWind = {
    Wind.Size = 119.0
    YearOfConstruction = 2024
    Degradation = [ 100.00; 99.83; 99.67; 99.50; 99.33; 99.17; 
                  99.00; 98.33; 97.67; 97.00; 96.33; 95.67; 
                  95.00; 94.00; 93.00; 92.00; 91.00; 90.00; 
                  89.00; 88.00; 87.00; 86.00; 85.00; 84.00; 
                  83.00; 82.00; 81.00; 80.00; 79.00 ]
}

// Dati Biomassa
type BiomassGasifier = {
    StrawMotorSize : float // MWel - Taglia motore a paglia - Mettere zero se no biomassa
    MinimumMotorLoad: float // %
    BiomassLHV: float // kJ/kg
    ElectricEfficiency: float // %
    AnnualAvailability: float // t/y
    Price: float // €/t
    Capex: float // €/MW - Capex tot (gassifier, engine, handling system, AUX)
    Bop: float // % del capex
}

let defaultBiomassGasifier = {
    StrawMotorSize = 4.0
    MinimumMotorLoad = 50.0
    BiomassLHV = 14000.0
    ElectricEfficiency = 20.0
    AnnualAvailability = 30_000.0
    Price = 45.0
    Capex = 3_000_000.0
    Bop = 7.0
}

// Batterie
type OverhaulBatteries = {
    AdditionEvery10Years: float // MWh
    AdditionSpecificPrice: float // €/KWh
}

type Battery = {
   YearOfConstruction: int
   Efficiency: float // %
   PowerOutput: float // MW
   HoursCapacity: int // h
   ExtraCapacity: float // % 
   SpecificPrice: float // €/kWh
   OverhaulBatteries: OverhaulBatteries
   Degradation: float list
}

let defaultBattery = {
    Battery.YearOfConstruction = 2024
    Efficiency = 85.0
    PowerOutput = 10.0
    HoursCapacity = 4
    ExtraCapacity = 0.0
    SpecificPrice = 220.0
    OverhaulBatteries = { AdditionEvery10Years = 0.0; AdditionSpecificPrice = 160.0}
    Degradation = [ 100.00; 95.00; 92.00; 90.00; 89.00;
                   87.00; 86.00; 85.00; 83.00; 82.00;
                   80.00; 79.00; 78.00; 76.00; 75.00;
                   73.00; 72.00; 70.00; 69.00; 67.00 ]
}

// Produzione H2

// Energy consumption at AC level, without auxiliaries
type EnergyConsumptionAC = {
   Load: float // %
   Consumption: float // kWh/Nm3
}

type Electrolyzers = {
    YearOfConstruction: int
    Lines: int
    PowerDcConsumption: float // MW
    NominalH2Production: float // kgH2/h
    PressureProductionH2: float // bar
    PressureNeedH2: float // bar
    Degradation: float // %
    WaterConsumption: float // L tapwater/kgH2
    WaterDischarge: float // %
    MinimumLoadOf1Line: float // %
    EnergyConsumption: EnergyConsumptionAC list
    PriceElectrolyzer: float // E/KWh
    PriceBOP: float // E/KWh
    PriceOther: float // E/KWh
    CoolingSystemConsumption: float // kW/(kg/h)
    GasManagementConsumption: float // kW/ kg/h)
    OverhaulElectrolyzer: float // €/MW electrolyzer
    H2CompressorSpecificConsumption: float option // kW/(kg/h)
}

let defaultElectrolyzers = {
    Electrolyzers.YearOfConstruction = 2024
    Lines = 3
    PowerDcConsumption = 17.1
    NominalH2Production = 330.0
    PressureProductionH2 = 30.0
    PressureNeedH2 = 80.0
    Degradation = 1.4
    WaterConsumption = 14.0
    WaterDischarge = 29.0
    MinimumLoadOf1Line = 20.0
    EnergyConsumption = [
        { Load = 100.0; Consumption = 4.8 }
        { Load = 80.0;  Consumption = 4.6 }
        { Load = 60.0;  Consumption = 4.4 }
        { Load = 40.0;  Consumption = 4.2 }
    ]
    PriceElectrolyzer = 808.0
    PriceBOP = 444.0
    PriceOther = 0.4
    CoolingSystemConsumption = 1.0
    GasManagementConsumption = 1.0
    OverhaulElectrolyzer = 200_000.0
    H2CompressorSpecificConsumption = None
}

let emptyElectrolyzers yearOfConstruction = {
    Electrolyzers.YearOfConstruction = yearOfConstruction
    Lines = 1
    PowerDcConsumption = 0.0
    NominalH2Production = 0.0
    PressureProductionH2 = 0.0
    PressureNeedH2 = 0.0
    Degradation = 0.0
    WaterConsumption = 0.0
    WaterDischarge = 0.0
    MinimumLoadOf1Line = 0.0
    EnergyConsumption = [
        { Load = 100.0; Consumption = 0.0 }
        { Load = 80.0;  Consumption = 0.0 }
        { Load = 60.0;  Consumption = 0.0 }
        { Load = 40.0;  Consumption = 0.0 }
    ]
    PriceElectrolyzer = 0.0
    PriceBOP = 0.0
    PriceOther = 0.0
    CoolingSystemConsumption = 0.0
    GasManagementConsumption = 0.0
    OverhaulElectrolyzer = 0.0
    H2CompressorSpecificConsumption = None
}

type Load = {
    MinimumH2Production: float // kg/h
}


type Oxigen = {
    Price: float // €/Kg
    Capex: float // €
    StoragePressure: float // bar
    Purity: float 
    CompressorConsumption: float
}

type PvWindNominalPower = {
    Day: int
    Month: int
    Hours: int
    PvOut1MWh: float
    WindOut1MWh: float
}

// System Input
type SystemInputs = {
    FirstYearOfOperationBP: int
    ManteinanceMonth: int
    PV: PV
    Wind: Wind
    BiomassGasifier: BiomassGasifier
    Battery: Battery
    Electrolyzers: Electrolyzers
    Load: Load
    Oxigen: Oxigen
    PvWindHourlyData: PvWindNominalPower list
}

let defaultSystemInput = {
    FirstYearOfOperationBP = 2024
    ManteinanceMonth = 0
    PV = defaultPV
    Wind = defaultWind
    BiomassGasifier = defaultBiomassGasifier
    Battery = defaultBattery
    Electrolyzers =  defaultElectrolyzers
    Load = {
        MinimumH2Production = 100.0 // kg/h
    }
    Oxigen = {
        Price = 0.04 // €/Kg
        Capex = 0.0 // €
        StoragePressure = 0.0 // bar
        Purity = 0.0
        CompressorConsumption = 0.0
    }
    PvWindHourlyData = []
}