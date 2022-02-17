module EconomicInputs

type VariableCosts =
    { ElectricityFromPvAndWind: float // €/MWh
      StrawConsumption: float // €/ton
      Water: float //€/ton
      VariableForMotor: float // €/MWh
      ElectricityToGrid: float } // €/MWh

type LCOE_PV_Inputs = 
    { CapexTotal: float // €/MW
      OpexTotal: float } // €/MW/y

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
