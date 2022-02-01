module Functions

open Inputs
open Outputs
open LinearRegression

//Battery
let BatteryWithDegradation (battery: Battery) =
    battery.Degradation
    |> List.mapi (fun index degradation ->
        { BatteryOutput.YearOfOperation = battery.YearOfConstruction + index
          MWh =
            float battery.HoursCapacity
            * battery.PowerOutput
            * (1.0 + battery.ExtraCapacity / 100.0)
            * degradation
            / 100.0
          MW =
            battery.PowerOutput
            * (1.0 + battery.ExtraCapacity / 100.0)
            * degradation
            / 100.0 })

let BatteryOutputByYear (inputs: SystemInputs) (yearOfOp: int) =
   let year = inputs.FirstYearOfOperationBP + yearOfOp - 1
   let bo = BatteryWithDegradation inputs.Battery
   let firstYear = bo.[0].YearOfOperation
   let lastYear = (bo |> List.last).YearOfOperation
   match year with
   | y when y < firstYear -> {BatteryOutput.YearOfOperation = y; MW = 0.0; MWh = 0.0 }
   | y when y > lastYear -> bo |> List.last
   | _ -> bo |> List.find (fun e -> e.YearOfOperation = year)

// PV and Wind Section
let findDegradationFactorByYear 
   (firstYearOfOperationBP: int) 
   (yearOfConstruction: int) 
   (degradationByYear: float list) =
   let index = firstYearOfOperationBP - yearOfConstruction;
   match index with
     | i when i < 0 -> 0.0
     | i when i > degradationByYear.Length -> (degradationByYear |> List.last) / 100.0
     | _ -> degradationByYear.[index] / 100.0

let PvAndWindWithDegradation (baseData: PvWindNominalPower list) 
    (pv: PV) (wind: Wind) 
    (yearOfOp: int) (firstYearOfOperationBP: int) =
    let selectedYear = firstYearOfOperationBP + yearOfOp - 1
    let pvDegFactor   = findDegradationFactorByYear selectedYear pv.YearOfConstruction pv.Degradation
    let windDegFactor = findDegradationFactorByYear selectedYear wind.YearOfConstruction wind.Degradation

    baseData
    |> List.map (fun data ->
        { PvWindOutput.Day = new System.DateTime(selectedYear, data.Month, data.Day, data.Hours, 0, 0)
          PvOut = data.PvOut1MWh * pvDegFactor * pv.Size
          WindOut = data.WindOut1MWh * windDegFactor * wind.Size })

// Questa funzione e quella successiva sono utili solo in fase di visualizzazione
let PvWithDegradationStats (baseData: PvWindNominalPower list) (pv: PV) =
    let allPV =
        baseData |> List.map (fun d -> d.PvOut1MWh)

    let min = allPV |> List.min
    let max = allPV |> List.max
    let total = allPV |> List.sum
    let average = allPV |> List.average

    let rows =
        pv.Degradation
        |> List.mapi (fun idx deg ->
            (pv.YearOfConstruction + idx,
             total * pv.Size * deg / 100.0,
             min * pv.Size * deg / 100.0,
             max * pv.Size * deg / 100.0,
             average * pv.Size * deg / 100.0))

    { EnergyDegradationOverYears.SerieName = "PV"
      Rows = rows }

let WindWithDegradationStats (baseData: PvWindNominalPower list) (wind: Wind) =
    let allWind =
        baseData |> List.map (fun d -> d.WindOut1MWh)

    let min = allWind |> List.min
    let max = allWind |> List.max
    let total = allWind |> List.sum
    let average = allWind |> List.average

    let rows =
        wind.Degradation
        |> List.mapi (fun idx deg ->
            (wind.YearOfConstruction + idx,
             total * wind.Size * deg / 100.0,
             min * wind.Size * deg / 100.0,
             max * wind.Size * deg / 100.0,
             average * wind.Size * deg / 100.0))

    { EnergyDegradationOverYears.SerieName = "Wind"
      Rows = rows }

// Biomass
let BiomassCalculator (biomassa: BiomassGasifier) =
    let tp =
        biomassa.StrawMotorSize
        / (biomassa.ElectricEfficiency / 100.0)

    let mbc1h =
        tp * 1000.0
        / (biomassa.BiomassLHV / 3600.0)
        / 1000.0

    { ThermalPower = tp
      MaxBiomassConsumptionOneHour = mbc1h
      BiomassConsumption = mbc1h / biomassa.StrawMotorSize
      MinimumLoad =
        biomassa.StrawMotorSize
        * biomassa.MinimumMotorLoad
        / 100.0 }


// Sezione Elettrolizzatori
let CalcElectrolizersTablesOutput (el_input: Electrolyzers) (el_output: ElectrolyzersOutput) (yearOfOp: int) =
    let year =
        el_input.YearOfConstruction + yearOfOp - 1

    let loads =
        el_output.EnergyConsumptionOutput
        |> List.map (fun ec -> ec.Load)

    let productionTemp =
        el_output.EnergyConsumptionOutput
        |> List.map (fun e ->
            el_input.PowerDcConsumption * 1000.0 * e.Load
            / 100.0
            / e.ConsumptionDC)
    //Ogni dieci anni i dati si resettano
    let counter =
        if yearOfOp % 10 = 0 then
            10
        else
            yearOfOp % 10

    let dcConsum1Line100perc =
        [ 2 .. counter ]
        |> List.map (fun e -> float e)
        |> List.fold
            (fun accumulator _ -> accumulator * (1.0 + el_input.Degradation / 100.0))
            (el_input.PowerDcConsumption * 1000.0)

    let dcConsum1Lines =
        el_output.EnergyConsumptionOutput
        |> List.map (fun e -> e.Load / 100.0 * dcConsum1Line100perc)

    let consDiff =
        el_output.EnergyConsumptionOutput
        |> List.map (fun ec -> ec.ConsumptionTot - ec.ConsumptionDC)

    let totConsum =
        (consDiff, dcConsum1Lines, productionTemp)
        |||> List.map3 (fun ec dc prod -> ec * prod + dc)

    let specCons =
        (totConsum, productionTemp)
        ||> List.map2 (fun cons prod -> cons / prod)

    let intercept, slope = Fit totConsum specCons

    { ElectrolizersTablesOutput.Year = year
      YearOfOp = yearOfOp
      Loads = loads
      DcConsum1LineWithDegradationAtPartialLoad = dcConsum1Lines
      TotConsum1LineWithDegradationAtPartialLoad = totConsum
      SpecificConsumption = specCons
      MinimumLoadOfOneLine =
        el_output.MinimumLoadOfOneLine
        * (specCons |> List.last)
        / 1000.0
      Slope = slope
      Intercect = intercept }


let ElectrolyzersWithDegradationStep1 (electrolyzers: Electrolyzers) =
    let hsvn = 11.128223495702 // HydrogenSpecificVolumeNTP

    { //First Entry
      ElectrolyzersOutput.PowerDcConsumptionTot =
        electrolyzers.PowerDcConsumption
        * float electrolyzers.Lines
      NominalH2ProductionTot =
        electrolyzers.NominalH2Production
        * float electrolyzers.Lines
      WaterConsumption = electrolyzers.WaterConsumption / 1000.0
      WaterDischarged =
        electrolyzers.WaterConsumption / 1000.0
        * electrolyzers.WaterDischarge
        / 100.0
      OxygenProduction = 8.0
      HydrogenSpecificVolumeNTP = hsvn
      MinimumLoadOfOneLine =
        electrolyzers.NominalH2Production
        * electrolyzers.MinimumLoadOf1Line
        / 100.0
      EnergyConsumptionOutput =
        electrolyzers.EnergyConsumption
        |> List.mapi (fun idx ec ->
            { EnergyConsumptionOutput.Load = ec.Load
              ConsumptionDC = hsvn * ec.Consumption * 0.976
              ConsumptionAC = hsvn * ec.Consumption
              ConsumptionTot = 0.0
              NominalH2Production =
                match idx with
                | 0 -> electrolyzers.NominalH2Production
                | _ -> electrolyzers.PowerDcConsumption * 10.0 * ec.Load / (hsvn * ec.Consumption * 0.976)
            })
      ConsumptionOverYears = []
    }

let ElectrolyzersWithDegradationStep2 (el_input: Electrolyzers) (el_output: ElectrolyzersOutput) =
    let h2sc =
       match el_input.H2CompressorSpecificConsumption with
       | Some v -> v
       | _ ->
          let compressionRatioH2 = el_input.PressureNeedH2 / el_input.PressureProductionH2
          let polinomialCoeff = [ -0.0000003; 0.00006; -0.0043; 0.1852; 0.1499 ]
          polinomialCoeff.[0] * compressionRatioH2 ** 4.0 +
          polinomialCoeff.[1] * compressionRatioH2 ** 3.0 +
          polinomialCoeff.[2] * compressionRatioH2 ** 2.0 +
          polinomialCoeff.[3] * compressionRatioH2 +
          polinomialCoeff.[4]
    
    let ec = 
       el_output.EnergyConsumptionOutput
       |> List.map (fun item -> 
          let op1 = item.ConsumptionAC * item.NominalH2Production
          let op2 = (el_input.CoolingSystemConsumption + el_input.GasManagementConsumption + h2sc) * item.NominalH2Production
          let op3 = el_input.OxigenCompressorConsumption * (item.NominalH2Production * el_output.OxygenProduction)
          let result = (op1 + op2 + op3) / item.NominalH2Production
          { item with ConsumptionTot = result}
       )

    { el_output with EnergyConsumptionOutput = ec}


let ElectrolyzersWithDegradationStep3 (el_input: Electrolyzers) (howManyYears: int) (el_output: ElectrolyzersOutput)  =
    let tableOut =
        [ 1 .. howManyYears ]
        |> List.map (fun year -> CalcElectrolizersTablesOutput el_input el_output year)

    { el_output with ConsumptionOverYears = tableOut }


let ElectrolyzersWithDegradation (electrolyzers: Electrolyzers) (howManyYears: int) =
    ElectrolyzersWithDegradationStep1 electrolyzers
    |> ElectrolyzersWithDegradationStep2 electrolyzers
    |> ElectrolyzersWithDegradationStep3 electrolyzers howManyYears

// Sezione Calculation Year

//Restituisce un valore nullo se prima dell'anno di costruzione
//oppure l'ultimo valore se dopo la curva di degradazione
let ElectrolyzersDataByYear (inputs: SystemInputs) (yearOfOp: int) =
   let year = inputs.FirstYearOfOperationBP + yearOfOp - 1
   let delta = year - inputs.Electrolyzers.YearOfConstruction + 1
   match delta with
   | d when d > 0 -> ElectrolyzersWithDegradation inputs.Electrolyzers delta
   | _ -> emptyElecOutput year

let CalculateYearRow
    (prevValue: CalculationYearRow)
    (currentValue: CalculationYearRow)
    (el_output: ElectrolyzersOutput)
    (lines: int)
    (manteinanceMonth: int)
    (strawMotorSize: float)
    (biomass: BiomassGasifierOutput)
    (batteryEfficency: float)
    (batteryOutput: BatteryOutput)
    (minimumH2Production: float)
    =
    let year = currentValue.Day.Year
    let month = currentValue.Day.Month

    let NpResNonProgrammable =
        currentValue.PvOut + currentValue.WindOut

    let MaxLoadCandidate =
        (el_output.ConsumptionOverYears
         |> List.find (fun e -> e.Year = year))
            .TotConsum1LineWithDegradationAtPartialLoad
            .Head
        * (float lines)

    let MaxLoad =
        if manteinanceMonth = month then
            0.0
        else
            MaxLoadCandidate

    let NpResMinusMaxLoad = NpResNonProgrammable - MaxLoad

    let PResProgrammable =
        match NpResMinusMaxLoad with
        | v when v > (biomass.MinimumLoad * -1000.0) -> 0.0
        | v when v > (strawMotorSize * -1000.0) -> v * -1.0
        | _ -> strawMotorSize * 1000.0

    let BiomassForEEProduction =
        PResProgrammable * biomass.BiomassConsumption

    let NpResAddPResMinusMaxLoad = NpResMinusMaxLoad + PResProgrammable

    let ToElectrolyzer =
        if NpResAddPResMinusMaxLoad < 0.0 then
            NpResNonProgrammable + PResProgrammable
        else
            MaxLoad

    let NeededFromStorage =
        if NpResAddPResMinusMaxLoad <= 0.0 then
            NpResAddPResMinusMaxLoad * -1.0
        else
            0.0

    let minOneLoadTemp =
        (el_output.ConsumptionOverYears
         |> List.find (fun e -> e.Year = year))
            .MinimumLoadOfOneLine
        * 1000.0

    let PotentiallyToStorage =
        if NpResAddPResMinusMaxLoad >= 0.0 then
            NpResAddPResMinusMaxLoad
        elif ToElectrolyzer < minOneLoadTemp then
            ToElectrolyzer
        else
            0.0

    let PotentiallyFromStorage =
        PotentiallyToStorage * batteryEfficency / 100.0

    let BatterySoC =
        let battMWh = batteryOutput.MWh * 1000.0
        let battMW = batteryOutput.MW * 1000.0

        let tempExp =
            if NeededFromStorage > 0.0
               && PotentiallyToStorage > 0.0
               && (prevValue.BatterySoC + ToElectrolyzer) < minOneLoadTemp then
                0.0
            else
                min NeededFromStorage battMW

        if ToElectrolyzer = 0.0
           && prevValue.BatterySoC < minOneLoadTemp then
            prevValue.BatterySoC
        else
            min
                battMWh
                (max
                    0.0
                    (prevValue.BatterySoC
                     + min PotentiallyFromStorage battMW
                     - tempExp))

    let ChargingDischarging = BatterySoC - prevValue.BatterySoC
    let Charging = if ChargingDischarging > 0.0 then ChargingDischarging else 0.0

    let ActualFromStorage =
        if ChargingDischarging <= 0.0 then
            ChargingDischarging * -1.0
        else
            0.0

    let EEToElectrolyser =
        let firstOp = ToElectrolyzer + ActualFromStorage

        let secondOp =
            if firstOp > minOneLoadTemp then
                0.0
            elif ToElectrolyzer < minOneLoadTemp then
                PotentiallyToStorage
            else
                0.0

        firstOp - secondOp

    let PotentialEEFromGrid = MaxLoad - EEToElectrolyser

    let LinesWorking =
        let elec100 =
            (el_output.ConsumptionOverYears
             |> List.find (fun e -> e.Year = year))
                .TotConsum1LineWithDegradationAtPartialLoad
                .Head

        let linesWorkingByEnergy =
           match EEToElectrolyser with
           | v when v = 0.0 -> 0
           | v when v < elec100 -> 1
           | v when v >= elec100 && v <= (elec100 * 2.0) -> 2
           | _ -> 3

        min linesWorkingByEnergy lines

    let EEToEachLine =
        if EEToElectrolyser = 0.0 then
            0.0
        else
            EEToElectrolyser / float LinesWorking

    let AreElectrolyzersWorkingUnderMiniumLoad =
        if LinesWorking > 0 && EEToEachLine < minOneLoadTemp then
            1
        else
            0

    let SpecificConsumption =
        let c =
            el_output.ConsumptionOverYears
            |> List.find (fun e -> e.Year = year)

        System.Math.Round EEToEachLine * c.Slope + c.Intercect

    let Module1 =
        if LinesWorking = 1 then
            EEToElectrolyser / SpecificConsumption
        else
            0.0

    let Module2 =
        if LinesWorking = 2 then
            EEToElectrolyser / SpecificConsumption
        else
            0.0

    let Module3 =
        if LinesWorking = 3 then
            EEToElectrolyser / SpecificConsumption
        else
            0.0

    // la produzione di idrogeno non può essere superiore al valore nominale
    // necessario per evitare errori di calcolo in vorgola mobile
    let TotalH2Production = min (Module1 + Module2 + Module3) el_output.NominalH2ProductionTot

    let HourWithoutH2Production = if TotalH2Production > 0.0 then 0 else 1

    let WaterConsumption =
        TotalH2Production * el_output.WaterConsumption

    let WaterDischarge =
        TotalH2Production * el_output.WaterDischarged

    let EnergyToGrid =
        if ChargingDischarging >= 0.0 then
            PotentiallyToStorage
            - (ChargingDischarging / (batteryEfficency / 100.0))
        else
            0.0

    let O2Production =
        TotalH2Production * el_output.OxygenProduction

    let N2Consumption = 0.0

    let H2ToBeProduced =
        if (minimumH2Production - TotalH2Production) <= 0.0 then
            0.0
        else
            minimumH2Production - TotalH2Production

    let EnergyFromGrid = H2ToBeProduced * SpecificConsumption

    { CalculationYearRow.Day = currentValue.Day
      PvOut = currentValue.PvOut
      WindOut = currentValue.WindOut
      NpResNonProgrammable = NpResNonProgrammable
      MaxLoad = MaxLoad
      NpResMinusMaxLoad = NpResMinusMaxLoad
      PResProgrammable = PResProgrammable
      BiomassForEEProduction = BiomassForEEProduction
      NpResAddPResMinusMaxLoad = NpResAddPResMinusMaxLoad
      ToElectrolyzer = ToElectrolyzer
      NeededFromStorage = NeededFromStorage
      PotentiallyToStorage = PotentiallyToStorage
      PotentiallyFromStorage = PotentiallyFromStorage
      BatterySoC = BatterySoC
      ChargingDischarging = ChargingDischarging
      Charging = Charging
      ActualFromStorage = ActualFromStorage
      EEToElectrolyser = EEToElectrolyser
      PotentialEEFromGrid = PotentialEEFromGrid
      LinesWorking = LinesWorking
      EEToEachLine = EEToEachLine
      AreElectrolyzersWorkingUnderMiniumLoad = AreElectrolyzersWorkingUnderMiniumLoad
      SpecificConsumption = SpecificConsumption
      Module1 = Module1
      Module2 = Module2
      Module3 = Module3
      TotalH2Production = TotalH2Production
      HourWithoutH2Production = HourWithoutH2Production
      WaterConsumption = WaterConsumption
      WaterDischarge = WaterDischarge
      EnergyToGrid = EnergyToGrid
      O2Production = O2Production
      N2Consumption = N2Consumption
      H2ToBeProduced = H2ToBeProduced
      EnergyFromGrid = EnergyFromGrid }

let CalculationYear (inputs: SystemInputs) (yearOfOp: int) =
    let initData =
        PvAndWindWithDegradation 
          inputs.PvWindHourlyData inputs.PV 
          inputs.Wind 
          yearOfOp 
          inputs.FirstYearOfOperationBP
        |> List.map (fun d ->
            { defaultCalculationYearRow with
                Day = d.Day
                PvOut = d.PvOut
                WindOut = d.WindOut })

    let el_output = ElectrolyzersDataByYear inputs yearOfOp

    let lines = inputs.Electrolyzers.Lines
    let manteinanceMonth = inputs.ManteinanceMonth
    let strawMotorSize = inputs.BiomassGasifier.StrawMotorSize
    let biomass = BiomassCalculator inputs.BiomassGasifier
    let batteryEfficency = inputs.Battery.Efficiency
    let batteryOutput = BatteryOutputByYear inputs yearOfOp
    let minimumH2Production = inputs.Load.MinimumH2Production

    let scanFunc (prevValue: CalculationYearRow) (currentValue: CalculationYearRow) =
        CalculateYearRow
            prevValue
            currentValue
            el_output
            lines
            manteinanceMonth
            strawMotorSize
            biomass
            batteryEfficency
            batteryOutput
            minimumH2Production

    let allData =
        initData
        |> List.scan scanFunc defaultCalculationYearRow
        |> List.skip 1

    { CalculationYearOutput.Rows = allData
      YearOfOperation = yearOfOp
      Year = allData.Head.Day.Year }
