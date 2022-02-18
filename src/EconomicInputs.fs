module EconomicInputs

type VariableCosts =
    { ElectricityFromPvAndWind: float // €/MWh
      StrawConsumption: float // €/ton
      Water: float //€/ton
      VariableForMotor: float // €/MWh
      ElectricityToGrid: float } // €/MWh


type LCOE_PV_Capex = // €/MWh
  { Authorizations: float
    Engineering: float
    PvModules: float
    Inverter: float
    Structures: float
    Bop: float
    CivilWorks: float
    Connections: float
    Assemblies: float
    Epcm: float
    Ground: float
    Other: float }
  member this.CapexTotal = 
    this.Authorizations +
    this.Engineering +
    this.PvModules +
    this.Inverter +
    this.Structures +
    this.Bop +
    this.CivilWorks +
    this.Connections +
    this.Assemblies +
    this.Epcm +
    this.Ground +
    this.Other

type LCOE_PV_Opex = // €/MWh/y
  { FullOem: float
    AssetManagement: float
    Insurance: float
    Trading: float
    Ground: float
    Other: float
  }
  member this.OpexTotal =
    this.FullOem +
    this.AssetManagement +
    this.Insurance +
    this.Trading +
    this.Ground +
    this.Other

type LCOE_PV_Inputs = 
    { LCOE_PV_Capex: LCOE_PV_Capex
      LCOE_PV_Opex: LCOE_PV_Opex }

type Financing =
    { // Equity + Debt deve essere uguale a 100
      Equity: float // %
      Debt: float } // %

type FinancialParameters =
    { LoanInterestRate: float // %
      CapitalDiscountRate: float // %
      InflationRate: float // %/y
      RepaimentPeriod: int // y
      EarningTax: float // %
      H2PriceEscalation: float // %/y
      AmortizationPeriod: int } // y

type FinancialInputs =
    { OEM_Costs: float // % CAPEX/y
      VariableCosts: VariableCosts
      LCOE_PV_Inputs: LCOE_PV_Inputs
      Financing: Financing
      FinancialParameters: FinancialParameters
      InitialInvestmentBreakdown: float list } // list %

let defaultFinancialInputs =
    { OEM_Costs = 1.25
      VariableCosts =
        { ElectricityFromPvAndWind = 50.0
          StrawConsumption = 45.0
          Water = 1.0
          VariableForMotor = 0.0
          ElectricityToGrid = 0.0 }
      LCOE_PV_Inputs =
        { 
          LCOE_PV_Capex = {
          Authorizations = 0.0
          Engineering = 0.0
          PvModules = 0.0
          Inverter = 0.0
          Structures = 0.0
          Bop = 0.0
          CivilWorks = 0.0
          Connections = 0.0
          Assemblies = 0.0
          Epcm = 0.0
          Ground = 0.0
          Other = 0.0
          }
          LCOE_PV_Opex = {
          FullOem = 0.0
          AssetManagement = 0.0
          Insurance = 0.0
          Trading = 0.0
          Ground = 0.0
          Other = 0.0
          }
        }
      Financing = { Equity = 80.0; Debt = 20.0 }
      FinancialParameters =
        { LoanInterestRate = 3.0
          CapitalDiscountRate = 6.0
          InflationRate = 0.0
          RepaimentPeriod = 20
          EarningTax = 24.0
          H2PriceEscalation = 0.0
          AmortizationPeriod = 10 }
      InitialInvestmentBreakdown = [ 30.0; 30.0; 40.0 ] }
