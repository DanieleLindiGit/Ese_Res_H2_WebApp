module EconomicFunctions

open Inputs
open Outputs
open EconomicInputs
open EconomicOutputs
open Functions
open FinancialFunctions

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
          (1.0 + inp.BiomassGasifier.Bop / 100.0)

       //Input!B76*Input!B74*Input!B137*Input!B136
       HydrogenStorage = 
          inp.Electrolyzers.NominalH2Production *
          float inp.Electrolyzers.Lines *
          float inp.HydrogenStorage.NumberOfHours * 
          inp.HydrogenStorage.StoragePrice

        //Input!B45*Input!B44
       OverhaulBatteriesCost = 
          inp.Battery.OverhaulBatteries.AdditionEvery10Years *
          inp.Battery.OverhaulBatteries.AdditionSpecificPrice
        
        // Input!B98*F14
       OverhaulElectrolizer = inp.Electrolyzers.OverhaulElectrolyzer * tec.ElectrolizerNominalStackPower

       //Input!B81*Electrolyzr_with_degradation!B2
       HydrogenCompressor = 
          inp.Electrolyzers.CapexHydrogenCompressor * 
          elec.NominalH2ProductionTot

       //Input!B126*Electrolyzr_with_degradation!B6
       OxigenCompressor = 
          inp.Oxigen.Capex * // Capex per produrre ossigeno (compressore) 
          elec.OxygenProduction * elec.NominalH2ProductionTot
      } 

let getBusinessPlanInput
   (inp: SystemInputs)
   (fin: FinancialInputs) =
       let el = ElectrolyzersWithDegradation inp.Electrolyzers 1
       let techAndEco = getTechAndEconomicalParameters inp el
       let capex = getCapexCost inp el techAndEco
       let oem = capex.Total * fin.OEM_Costs / 100.0
       let f_total = capex.Total

       {
          BusinessPlanInput.TechAndEconomicalParameters = techAndEco
          CapexCost = capex
          OEM_Costs = oem
          FinancialInputs = fin
          FinancingTotal = f_total
          CostructionYears = fin.InitialInvestmentBreakdown.Length
       }

let getConstructionYears (inp:BusinessPlanInput) (sys: SystemInputs) =
   let firstYearOfConstruction = sys.FirstYearOfOperationBP - inp.FinancialInputs.InitialInvestmentBreakdown.Length
   let totEquity = inp.FinancingTotal * inp.FinancialInputs.Financing.Equity / 100.0 / ThousandsOrUnits
   let totDebt   = inp.FinancingTotal * inp.FinancialInputs.Financing.Debt  / 100.0 / ThousandsOrUnits
   let interest = inp.FinancialInputs.FinancialParameters.LoanInterestRate
   let cyears = 
      inp.FinancialInputs.InitialInvestmentBreakdown |> List.mapi (fun idx iibd ->
      let debt = totDebt * iibd / 100.0
      {
         ConstructionYear.TotalYear = idx + 1
         Year = firstYearOfConstruction + idx
         CyEquity = totEquity * iibd / 100.0
         CyDebt = debt
         CyIDC = debt * interest / 200.0 
         FCF = -totEquity * iibd / 100.0
      })

   let cumulativeDebt = // somma con accumulo e rimuove ultimo elemento della lista
      (0.0, cyears) ||> List.scan (fun acc current ->
      current.CyDebt + current.CyIDC + acc)
      |> List.rev
      |> List.tail
      |> List.rev

   (cyears, cumulativeDebt) ||> List.map2 (fun cy cumDebt ->
      {
         cy with CyIDC = cy.CyIDC + cumDebt * interest / 100.0
      })

let getTotalDebt (constructionYears: ConstructionYear list) =
   constructionYears |> List.map (fun v -> v.CyDebt + v.CyIDC) |> List.sum

let getHydrogenRevenues hydrogenProduction lcoh h2PriceEscalation (totalYear: int) =
   (hydrogenProduction * lcoh *
   ((1.0 + h2PriceEscalation / 100.0) ** (float totalYear - 1.0))) /
   ThousandsOrUnits

let resetYearAnalisys (inp: YearAnalysis) (lcoh: float) =
   let HydrogenRevenues = getHydrogenRevenues inp.HydrogenProduction lcoh inp.H2PriceEscalation inp.TotalYear
   let TotalRevenues = HydrogenRevenues + inp.OxigenRevenues + inp.ExtraEEtoGridRevenues
   let EBIT = TotalRevenues - inp.Amortization - inp.TotalCosts
   let EBT = EBIT + inp.DebtInterest
   let Tax = 
      if EBT < 0.0
      then 0.0
      else EBT * inp.EarningTax / 100.0

   let EAT = EBT - Tax
   let FCF = inp.Amortization + inp.DebtReimbursement + EAT
   {
      inp with 
         HydrogenRevenues = HydrogenRevenues
         TotalRevenues = TotalRevenues
         EBIT = EBIT
         EBT = EBT
         Tax = Tax
         EAT = EAT
         FCF = FCF
   }


let getYearAnalysis 
   (sys: SystemInputs) 
   (inp:BusinessPlanInput) 
   (yearOfOp: int) 
   (totalDebt: float) =
   let TotalYear = inp.FinancialInputs.InitialInvestmentBreakdown.Length + yearOfOp
   let OperationalYear = yearOfOp
   let Year = sys.FirstYearOfOperationBP + yearOfOp - 1

   let Amortization =
      if yearOfOp <= inp.FinancialInputs.FinancialParameters.AmortizationPeriod
      then inp.FinancingTotal / float inp.FinancialInputs.FinancialParameters.AmortizationPeriod / ThousandsOrUnits
      else 0.0

   let H2PriceEscalation = inp.FinancialInputs.FinancialParameters.H2PriceEscalation
   let EarningTax = inp.FinancialInputs.FinancialParameters.EarningTax

   let inflationUpToDate = 
      (1.0 + inp.FinancialInputs.FinancialParameters.InflationRate / 100.0) ** (float TotalYear - 1.0)
   
   let cy = CalculationYear sys yearOfOp

   let HydrogenProduction = cy.Rows |> List.sumBy (fun v -> v.TotalH2Production)
   let OxigenProduction = cy.Rows |> List.sumBy (fun v -> v.O2Production)
   let ExtraEEtoGrid = (cy.Rows |> List.sumBy (fun v -> v.EnergyToGrid)) / 1000.0
   let ToElectrolyzer = cy.Rows |> List.sumBy (fun v -> v.ToElectrolyzer)
   let PResProgrammable = cy.Rows |> List.sumBy (fun v -> v.PResProgrammable)
   let BatteryCharging = cy.Rows |> List.sumBy (fun v -> v.Charging)
   let WaterConsumption = cy.Rows |> List.sumBy (fun v -> v.WaterConsumption)
   let BiomassForEEProduction = cy.Rows |> List.sumBy (fun v -> v.BiomassForEEProduction)

   let OxigenRevenues = OxigenProduction * sys.Oxigen.Price / ThousandsOrUnits
   let ExtraEEtoGridRevenues = ExtraEEtoGrid * inp.FinancialInputs.VariableCosts.ElectricityToGrid / ThousandsOrUnits
   let FixedCosts = 
      inp.OEM_Costs / ThousandsOrUnits * inflationUpToDate
   
   let ElectricityCost = 
      (ToElectrolyzer - PResProgrammable +
      BatteryCharging / (sys.Battery.Efficiency / 100.0)) *
      inp.FinancialInputs.VariableCosts.ElectricityFromPvAndWind / 1000.0 /
      ThousandsOrUnits * inflationUpToDate

   let WaterCost = 
      (WaterConsumption * inp.FinancialInputs.VariableCosts.Water * inflationUpToDate)
      / ThousandsOrUnits

   let BiomassCost = 
      (BiomassForEEProduction * inp.FinancialInputs.VariableCosts.StrawConsumption * 
      inflationUpToDate) / 1000.0 / ThousandsOrUnits

   let DebtReimbursement = 
      PrestitoRata 
         (inp.FinancialInputs.FinancialParameters.LoanInterestRate / 100.0)
         OperationalYear
         inp.FinancialInputs.FinancialParameters.RepaimentPeriod
         totalDebt

   let DebtInterest = 
      Interessi 
         (inp.FinancialInputs.FinancialParameters.LoanInterestRate / 100.0)
         OperationalYear
         inp.FinancialInputs.FinancialParameters.RepaimentPeriod
         totalDebt
   
   let TotalCosts = FixedCosts + ElectricityCost + WaterCost + BiomassCost
   
   let item = {
      YearAnalysis.TotalYear = TotalYear
      OperationalYear = OperationalYear
      Year = Year

      Amortization = Amortization
      H2PriceEscalation = H2PriceEscalation
      EarningTax = EarningTax

      HydrogenProduction = HydrogenProduction
      HydrogenRevenues = 0.0
      OxigenProduction = OxigenProduction
      OxigenRevenues = OxigenRevenues
      ExtraEEtoGrid = ExtraEEtoGrid
      ExtraEEtoGridRevenues = ExtraEEtoGridRevenues

      FixedCosts = FixedCosts
      ToElectrolyzer = ToElectrolyzer
      PResProgrammable = PResProgrammable
      BatteryCharging = BatteryCharging
      ElectricityCost = ElectricityCost
      WaterConsumption = WaterConsumption
      WaterCost = WaterCost
      BiomassForEEProduction = BiomassForEEProduction
      BiomassCost = BiomassCost

      DebtReimbursement = DebtReimbursement
      DebtInterest = DebtInterest
      Tax = 0.0

      TotalRevenues = 0.0
      TotalCosts = TotalCosts
      EBIT = 0.0
      EBT = 0.0
      EAT = 0.0
      FCF = 0.0
   }
   resetYearAnalisys item IpotesiInizialeLCOH

let getNPV_IRR_CashFlow (cy: ConstructionYear list) (ya: YearAnalysis list) (interestRate: float) =
   let cashFlow = (cy |> List.map (fun v -> v.FCF)) @ (ya |> List.map (fun v -> v.FCF))
   (NPV interestRate cashFlow, IRR cashFlow, cashFlow)
   

let getBusinessPlanOutput 
   (inp: SystemInputs)
   (fin: FinancialInputs) =
   let bi = getBusinessPlanInput inp fin
   let cy = getConstructionYears bi inp
   let totalDebt = getTotalDebt cy

   let repaimentRange = [1 .. bi.FinancialInputs.FinancialParameters.RepaimentPeriod]

   let ya =
      repaimentRange |> List.map (fun idx -> 
         getYearAnalysis inp bi idx totalDebt
      )

   let npv, irr, cf = getNPV_IRR_CashFlow cy ya bi.FinancialInputs.FinancialParameters.LoanInterestRate
   
   {
      BusinessPlanOutput.LCOH = IpotesiInizialeLCOH
      BpNPV = npv
      BpIRR = irr
      CashFlow = cf
      TotalDebt = totalDebt
      LoanInterestRate = bi.FinancialInputs.FinancialParameters.LoanInterestRate
      BusinessPlanInput = bi
      ConstructionYears = cy
      YearsAnalysis = ya
   }

let ricalculateBusinessPlan (bpo: BusinessPlanOutput) (lcoh: float) =
   let newData = bpo.YearsAnalysis |> List.map (fun ya -> resetYearAnalisys ya lcoh)
   let npv, irr, cf = getNPV_IRR_CashFlow bpo.ConstructionYears newData bpo.LoanInterestRate

   {
      bpo with
         LCOH = lcoh
         BpNPV = npv
         BpIRR = irr
         CashFlow = cf
         YearsAnalysis = newData
   }

let optimizeLCOH (bpo: BusinessPlanOutput) (min: float) max step =
   let lcohs = [min .. step .. max]
   let npv = 
      lcohs 
      |> List.map (fun lcoh -> ricalculateBusinessPlan bpo lcoh) 
      |> List.map (fun v -> v.BpNPV)
      |> List.map abs

   let values = List.zip lcohs npv

   (*values 
      |> List.iter (fun v ->
      let a, b = v
      printfn "LCOH = %.4f NPV = %.0f" a b)*)
   
   let minLcoh =
      values 
      |> List.minBy snd
      |> fst
   
   minLcoh

let finalBusinessPlan
   (inp: SystemInputs)
   (fin: FinancialInputs) =

   let bpo = getBusinessPlanOutput inp fin

   let step1 = optimizeLCOH bpo 1.0 10.0 1.0
   let step2 = optimizeLCOH bpo (step1-0.5) (step1+0.5) 0.1
   let step3 = optimizeLCOH bpo (step2 - 0.05) (step2 + 0.05) 0.01
   let step4 = optimizeLCOH bpo (step3 - 0.005) (step3 + 0.005) 0.001
   let step5 = optimizeLCOH bpo (step4 - 0.0005) (step4 + 0.0005) 0.0001
   ricalculateBusinessPlan bpo step5