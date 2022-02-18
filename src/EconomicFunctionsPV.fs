module EconomicFunctionsPV

open Inputs
open Outputs
open EconomicInputs
open EconomicOutputsPV
open Functions
open FinancialFunctions

let getBusinessPlanInputPV (fin: FinancialInputs) (sys: SystemInputs) (lc: LCOE_PV_Inputs) =
    // Trasformo gli input in base alla size del PV
    { BusinessPlanInputPV.LCOE_PV_Inputs = 
        {
          LCOE_PV_Inputs.CapexTotal = lc.CapexTotal * sys.PV.Size
          OpexTotal = lc.OpexTotal * sys.PV.Size
        }
      FinancialInputs = fin
      FinancingTotal = lc.CapexTotal * sys.PV.Size
      CostructionYears = fin.InitialInvestmentBreakdown.Length }


let getConstructionYearsPV (inp: BusinessPlanInputPV) (sys: SystemInputs) =
    let firstYearOfConstruction =
        sys.FirstYearOfOperationBP
        - inp.FinancialInputs.InitialInvestmentBreakdown.Length

    let totEquity =
        inp.FinancingTotal
        * inp.FinancialInputs.Financing.Equity
        / 100.0
        / ThousandsOrUnits


    let totDebt =
        inp.FinancingTotal
        * inp.FinancialInputs.Financing.Debt
        / 100.0
        / ThousandsOrUnits

    let interest = inp.FinancialInputs.FinancialParameters.LoanInterestRate

    let cyears =
        inp.FinancialInputs.InitialInvestmentBreakdown
        |> List.mapi (fun idx iibd ->
            let debt = totDebt * iibd / 100.0

            { ConstructionYearPV.TotalYear = idx + 1
              Year = firstYearOfConstruction + idx
              CyEquity = totEquity * iibd / 100.0
              CyDebt = debt
              CyIDC = debt * interest / 200.0
              FCF = -totEquity * iibd / 100.0 })

    let cumulativeDebt = // somma con accumulo e rimuove ultimo elemento della lista
        (0.0, cyears)
        ||> List.scan (fun acc current -> current.CyDebt + current.CyIDC + acc)
        |> List.rev
        |> List.tail
        |> List.rev

    (cyears, cumulativeDebt)
    ||> List.map2 (fun cy cumDebt -> { cy with CyIDC = cy.CyIDC + cumDebt * interest / 100.0 })

let getTotalDebtPV (constructionYears: ConstructionYearPV list) =
    constructionYears
    |> List.map (fun v -> v.CyDebt + v.CyIDC)
    |> List.sum

let getPVRevenues pvProduction lcoe =
    (pvProduction
     * lcoe )
    / ThousandsOrUnits

let resetYearAnalisysPV (inp: YearAnalysisPV) (lcoe: float) =
    let PVRevenues =
        getPVRevenues inp.PVEnergyProduced lcoe

    let EBIT = PVRevenues - inp.Amortization - inp.TotalCosts
    let EBT = EBIT + inp.DebtInterest

    let Tax =
        if EBT < 0.0 then
            0.0
        else
            EBT * inp.EarningTax / 100.0

    let EAT = EBT - Tax
    let FCF = inp.Amortization + inp.DebtReimbursement + EAT

    { inp with
        PVRevenues = PVRevenues
        TotalRevenues = PVRevenues
        EBIT = EBIT
        EBT = EBT
        Tax = Tax
        EAT = EAT
        FCF = FCF }

let getYearAnalysisPV (sys: SystemInputs) (inp: BusinessPlanInputPV) (yearOfOp: int) (totalDebt: float) =
    let TotalYear =
        inp.FinancialInputs.InitialInvestmentBreakdown.Length
        + yearOfOp

    let OperationalYear = yearOfOp
    let Year = sys.FirstYearOfOperationBP + yearOfOp - 1

    let Amortization =
        if yearOfOp
           <= inp.FinancialInputs.FinancialParameters.AmortizationPeriod then
            inp.FinancingTotal
            / float inp.FinancialInputs.FinancialParameters.AmortizationPeriod
            / ThousandsOrUnits
        else
            0.0

    let EarningTax = inp.FinancialInputs.FinancialParameters.EarningTax

    let inflationUpToDate =
        (1.0
         + inp.FinancialInputs.FinancialParameters.InflationRate
           / 100.0)
        ** (float TotalYear - 1.0)

    let opex = inp.LCOE_PV_Inputs.OpexTotal / ThousandsOrUnits * inflationUpToDate

    let pvStats = PvWithDegradationStats sys.PvWindHourlyData sys.PV
    let candidatePV = pvStats.Rows |> List.tryFind (fun (year, _, _, _, _) -> year = Year)
    let totalPvProduction =
        match candidatePV with
        | Some (_, total, _, _, _) -> total / 1000.0
        | None -> 0.0

    let DebtReimbursement =
        PrestitoRata
            (inp.FinancialInputs.FinancialParameters.LoanInterestRate
             / 100.0)
            OperationalYear
            inp.FinancialInputs.FinancialParameters.RepaimentPeriod
            totalDebt

    let DebtInterest =
        Interessi
            (inp.FinancialInputs.FinancialParameters.LoanInterestRate
             / 100.0)
            OperationalYear
            inp.FinancialInputs.FinancialParameters.RepaimentPeriod
            totalDebt

    let item =
        { YearAnalysisPV.TotalYear = TotalYear
          OperationalYear = OperationalYear
          Year = Year

          Amortization = Amortization
          EarningTax = EarningTax

          PVEnergyProduced = totalPvProduction
          PVRevenues = getPVRevenues totalPvProduction IpotesiInizialeLCOE_PV

          OpexCost = opex

          DebtReimbursement = DebtReimbursement
          DebtInterest = DebtInterest
          Tax = 0.0

          TotalRevenues = 0.0
          TotalCosts = opex
          EBIT = 0.0
          EBT = 0.0
          EAT = 0.0
          FCF = 0.0 }

    resetYearAnalisysPV item IpotesiInizialeLCOE_PV

let getNPV_IRR_CashFlow (cy: ConstructionYearPV list) (ya: YearAnalysisPV list) (interestRate: float) =
    let cashFlow =
        (cy |> List.map (fun v -> v.FCF))
        @ (ya |> List.map (fun v -> v.FCF))

    printfn "Cash Flow = %A" (cashFlow |> List.map (fun v -> System.Math.Round(v, 0)))
    printfn "Interest = %.2f" interestRate
    printfn "NPV = %.2f" (NPV interestRate cashFlow)

    (NPV interestRate cashFlow, IRR cashFlow, cashFlow)

let getBusinessPlanOutputPV (inp: SystemInputs) (fin: FinancialInputs) (lc: LCOE_PV_Inputs) =
    let bi = getBusinessPlanInputPV fin inp lc
    let cy = getConstructionYearsPV bi inp
    let totalDebt = getTotalDebtPV cy

    let repaimentRange = [ 1 .. bi.FinancialInputs.FinancialParameters.RepaimentPeriod ]

    let ya =
        repaimentRange
        |> List.map (fun idx -> getYearAnalysisPV inp bi idx totalDebt)

    let npv, irr, cf =
        getNPV_IRR_CashFlow cy ya bi.FinancialInputs.FinancialParameters.CapitalDiscountRate

    { BusinessPlanOutputPV.LCOE = IpotesiInizialeLCOE_PV
      BpNPV = npv
      BpIRR = irr
      CashFlow = cf
      TotalDebt = totalDebt
      LoanInterestRate = bi.FinancialInputs.FinancialParameters.LoanInterestRate
      CapitalDiscountRate = bi.FinancialInputs.FinancialParameters.CapitalDiscountRate
      BusinessPlanInput = bi
      ConstructionYears = cy
      YearsAnalysis = ya }

let ricalculateBusinessPlanPV (bpo: BusinessPlanOutputPV) (lcoe: float) =
    let newData =
        bpo.YearsAnalysis
        |> List.map (fun ya -> resetYearAnalisysPV ya lcoe)

    let npv, irr, cf =
        getNPV_IRR_CashFlow bpo.ConstructionYears newData bpo.CapitalDiscountRate

    { bpo with
        LCOE = lcoe
        BpNPV = npv
        BpIRR = irr
        CashFlow = cf
        YearsAnalysis = newData }

let optimizeLCOE_PV (bpo: BusinessPlanOutputPV) (min: float) max step =
    let lcoes = [ min..step..max ]

    let npv =
        lcoes
        |> List.map (fun lcoe -> ricalculateBusinessPlanPV bpo lcoe)
        |> List.map (fun v -> v.BpNPV)
        |> List.map abs

    let values = List.zip lcoes npv

    (*values
      |> List.iter (fun v ->
      let a, b = v
      printfn "LCOH = %.4f NPV = %.0f" a b)*)

    let minLcoe = values |> List.minBy snd |> fst

    minLcoe

let finalBusinessPlanPV (inp: SystemInputs) (fin: FinancialInputs) (lc: LCOE_PV_Inputs) =

    let bpo = getBusinessPlanOutputPV inp fin lc

    let step0 = optimizeLCOE_PV bpo 10.0 100.0 10.0
    let step1 = optimizeLCOE_PV bpo (step0 - 5.0) (step0 + 5.0) 1.0
    let step2 = optimizeLCOE_PV bpo (step1 - 0.5) (step1 + 0.5) 0.1
    let step3 = optimizeLCOE_PV bpo (step2 - 0.05) (step2 + 0.05) 0.01
    let step4 = optimizeLCOE_PV bpo (step3 - 0.005) (step3 + 0.005) 0.001
    let step5 = optimizeLCOE_PV bpo (step4 - 0.0005) (step4 + 0.0005) 0.0001
    ricalculateBusinessPlanPV bpo step5
