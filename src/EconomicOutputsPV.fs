module EconomicOutputsPV

open EconomicInputs

// Questa costante mostra gli outuput monetari in migliaia
let ThousandsOrUnits = 1000.0

let IpotesiInizialeLCOE_PV = 40.0

type BusinessPlanInputPV =
    { LCOE_PV_Inputs: LCOE_PV_Inputs
      FinancialInputs: FinancialInputs
      FinancingTotal: float // €
      CostructionYears: int } // quanti anni prima che sia operativa

type ConstructionYearPV =
    { TotalYear: int
      Year: int

      CyEquity: float
      CyDebt: float
      CyIDC: float // Interest Under Construction
      FCF: float }

type YearAnalysisPV =
    { TotalYear: int
      OperationalYear: int
      Year: int

      Amortization: float // €
      EarningTax: float

      // REVENUES
      PVEnergyProduced: float
      PVRevenues: float // €

      // COSTS
      OpexCost: float // €

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
      FCF: float }

type BusinessPlanOutputPV =
    { LCOE: float // €/Kg
      BpNPV: float // €
      BpIRR: float // €
      CashFlow: float list
      TotalDebt: float
      LoanInterestRate: float
      CapitalDiscountRate: float
      BusinessPlanInput: BusinessPlanInputPV

      ConstructionYears: ConstructionYearPV list
      YearsAnalysis: YearAnalysisPV list

     }