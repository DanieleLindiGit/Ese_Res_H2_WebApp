module EconomicOutputs

open EconomicInputs

// Questa costante mostra gli outuput monetari in migliaia
let ThousandsOrUnits = 1000.0

let IpotesiInizialeLCOH = 5.0

type TechAndEconomicalParameters = {
    ElectrolizerNominalStackPower: float // MW
    BatteriesEnergyCapacity: float // MWh
}

type CapexCost = 
   {
    ElectrolizerEquipment: float // €
    Batteries: float // €
    StrawGasifier: float // €
    HydrogenStorage: float // €
    OverhaulBatteriesCost: float // €
    OverhaulElectrolizer: float // €
    HydrogenCompressor: float // €
    OxigenCompressor: float // €
   }
   member this.Total = 
      this.ElectrolizerEquipment + 
      this.Batteries +
      this.StrawGasifier +
      this.HydrogenStorage +
      this.OverhaulBatteriesCost +
      this.OverhaulElectrolizer +
      this.HydrogenCompressor +
      this.OxigenCompressor

// tipo che raggruppa i vari dati di input del business plan
type BusinessPlanInput = {
    TechAndEconomicalParameters: TechAndEconomicalParameters
    CapexCost: CapexCost
    OEM_Costs: float
    FinancialInputs: FinancialInputs
    FinancingTotal: float // €
    CostructionYears: int // quanti anni prima che sia operativa
}

type ConstructionYear = {
    TotalYear: int
    Year: int

    CyEquity: float
    CyDebt: float
    CyIDC: float // Interest Under Construction
    FCF: float
}

let defaultConstructionYear = {
    ConstructionYear.TotalYear = 0
    Year = 0
    CyEquity = 0.0
    CyDebt = 0.0
    CyIDC = 0.0
    FCF = 0.0
}

type YearAnalysis = 
   {
    TotalYear: int
    OperationalYear: int
    Year: int

    Amortization: float // €
    H2PriceEscalation: float
    EarningTax: float

    // REVENUES
    HydrogenProduction: float // kgH2 SUM OF TotalH2Production
    HydrogenRevenues: float // €
    OxigenProduction: float // kg SUM OF O2Production
    OxigenRevenues: float // €
    ExtraEEtoGrid: float // MWh SUM OF EnergyToGrid / 1000
    ExtraEEtoGridRevenues: float // €

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

    //Pure Financial Calculation
    DebtReimbursement: float
    DebtInterest: float
    Tax: float

    // Calculated from above
    TotalRevenues: float
    TotalCosts: float
    EBIT: float
    EBT: float
    EAT: float
    FCF: float
   }


type BusinessPlanOutput = {
    LCOH: float // €/Kg
    BpNPV: float // €
    BpIRR: float // €
    CashFlow: float list
    TotalDebt: float
    LoanInterestRate: float

    ConstructionYears: ConstructionYear list
    YearsAnalysis: YearAnalysis list

}