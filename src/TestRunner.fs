module TestRunner

open Inputs
open EconomicInputs

type ProjectInput =
    { SystemInputs: SystemInputs
      FinancialInputs: FinancialInputs }

// Restituisce le possibile combinazioni in un vettore di matrici
// ESEMPIO: CartesianProduct [[1;2];[3;4;5];[6;7]] |> Seq.toList
// RETURNS: [[1; 3; 6]; [1; 3; 7]; [1; 4; 6]; [1; 4; 7]; [1; 5; 6];
//           [1; 5; 7]; [2; 3; 6]; [2; 3; 7]; [2; 4; 6]; [2; 4; 7];
//           [2; 5; 6]; [2; 5; 7]]
let rec CartesianProduct LL =
    match LL with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in CartesianProduct Ls -> x :: xs
        }

let ParseFloatValue (v: string) =
    float (
        v
            .Replace(".", System.String.Empty)
            .Replace(",", ".")
    )

let SplitByChar (v: string) (sep: string) =
    v.Split([| sep |], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray

let SplitRange v = SplitByChar v ".."

let SplitLines v = SplitByChar v "\n"

let SplitNameAndValues v =
    let tuple = SplitByChar v "="
    (tuple.[0].Trim(), tuple.[1])

let GetFloatRange v =
    let vs = SplitRange v |> List.map ParseFloatValue

    match vs.Length with
    | 1 -> vs
    | _ -> [ vs.[0] .. vs.[1] .. vs.[2] ]

let GetSingleParameterFromString v =
    let (name, s_values) = SplitNameAndValues v
    let values = GetFloatRange s_values
    (name, values)

let rowIsValid (v: string) =
    let mustSkip =
        v.StartsWith("#")
        || v.Length < 5
        || v.Contains("SKIP")

    not mustSkip

let removeComments (v: string) =
    let idx = v.IndexOf '#'

    match idx with
    | _ when idx > 0 -> v.Substring(0, idx)
    | _ -> v

let GetParametersFromString v =
    SplitLines v
    |> List.filter rowIsValid
    |> List.map removeComments
    |> List.map GetSingleParameterFromString

let GetParametersCombinations v =
    let name_and_values = GetParametersFromString v
    let parametersNames = name_and_values |> List.map fst
    let values = name_and_values |> List.map snd
    let combination = CartesianProduct values

    seq {
        for c in combination do
            List.zip parametersNames c
    }

let CombinationToString (line: (string * float) list) =
    let singleParam (item: string * float) =
        let (param, value) = item
        sprintf "%s = %.2f" param value

    line |> List.map singleParam |> String.concat " | "

let testString =
    """
# This is a comment
param0 = SKIP
param1 = 1 .. 1 .. 3 # comment inline
param2 = 5 .. 1 .. 7
param3 = 8 .. 1 .. 10
"""

let GetProjectInputFromParameter (inp: ProjectInput) (parameter: string * float) =
    let (pname, pvalue) = parameter

    match pname with
    | "PV.Size" -> { inp with SystemInputs = { inp.SystemInputs with PV = { inp.SystemInputs.PV with Size = pvalue } } }
    | "PV.YearOfConstruction" ->
        { inp with
            SystemInputs = { inp.SystemInputs with PV = { inp.SystemInputs.PV with YearOfConstruction = int pvalue } } }
    | "Wind.Size" ->
        { inp with SystemInputs = { inp.SystemInputs with Wind = { inp.SystemInputs.Wind with Size = pvalue } } }
    | "Wind.YearOfConstruction" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Wind = { inp.SystemInputs.Wind with YearOfConstruction = int pvalue } } }

    | "BiomassGasifier.StrawMotorSize" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    BiomassGasifier = { inp.SystemInputs.BiomassGasifier with StrawMotorSize = pvalue } } }
    | "BiomassGasifier.MinimumMotorLoad" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    BiomassGasifier = { inp.SystemInputs.BiomassGasifier with MinimumMotorLoad = pvalue } } }
    | "BiomassGasifier.BiomassLHV" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with BiomassLHV = pvalue } } }
    | "BiomassGasifier.ElectricEfficiency" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    BiomassGasifier = { inp.SystemInputs.BiomassGasifier with ElectricEfficiency = pvalue } } }
    | "BiomassGasifier.AnnualAvailability" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    BiomassGasifier = { inp.SystemInputs.BiomassGasifier with AnnualAvailability = pvalue } } }
    | "BiomassGasifier.Price" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Price = pvalue } } }
    | "BiomassGasifier.Capex" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Capex = pvalue } } }
    | "BiomassGasifier.Bop" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with BiomassGasifier = { inp.SystemInputs.BiomassGasifier with Bop = pvalue } } }

    | "Battery.YearOfConstruction" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with YearOfConstruction = int pvalue } } }
    | "Battery.Efficiency" ->
        { inp with
            SystemInputs = { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with Efficiency = pvalue } } }
    | "Battery.PowerOutput" ->
        { inp with
            SystemInputs = { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with PowerOutput = pvalue } } }
    | "Battery.HoursCapacity" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with HoursCapacity = int pvalue } } }
    | "Battery.ExtraCapacity" ->
        { inp with
            SystemInputs = { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with ExtraCapacity = pvalue } } }
    | "Battery.SpecificPrice" ->
        { inp with
            SystemInputs = { inp.SystemInputs with Battery = { inp.SystemInputs.Battery with SpecificPrice = pvalue } } }
    | "OverhaulBatteries.AdditionEvery10Years" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    Battery =
                        { inp.SystemInputs.Battery with
                            OverhaulBatteries =
                                { inp.SystemInputs.Battery.OverhaulBatteries with AdditionEvery10Years = pvalue } } } }
    | "OverhaulBatteries.AdditionSpecificPrice" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with
                    Battery =
                        { inp.SystemInputs.Battery with
                            OverhaulBatteries =
                                { inp.SystemInputs.Battery.OverhaulBatteries with AdditionSpecificPrice = pvalue } } } }

    | "Electrolyzers.YearOfConstruction" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with YearOfConstruction = int pvalue } } }
    | "Electrolyzers.Lines" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with Lines = int pvalue } } }
    | "Electrolyzers.PowerDcConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PowerDcConsumption = pvalue } } }
    | "Electrolyzers.NominalH2Production" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with NominalH2Production = pvalue } } }
    | "Electrolyzers.PressureProductionH2" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PressureProductionH2 = pvalue } } }
    | "Electrolyzers.PressureNeedH2" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PressureNeedH2 = pvalue } } }
    | "Electrolyzers.Degradation" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with Degradation = pvalue } } }
    | "Electrolyzers.WaterConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with WaterConsumption = pvalue } } }
    | "Electrolyzers.WaterDischarge" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with WaterDischarge = pvalue } } }
    | "Electrolyzers.MinimumLoadOf1Line" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with MinimumLoadOf1Line = pvalue } } }
    | "Electrolyzers.PriceElectrolyzer" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceElectrolyzer = pvalue } } }
    | "Electrolyzers.PriceBOP" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceBOP = pvalue } } }
    | "Electrolyzers.PriceOther" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with PriceOther = pvalue } } }
    | "Electrolyzers.CapexHydrogenCompressor" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with CapexHydrogenCompressor = pvalue } } }
    | "Electrolyzers.CoolingSystemConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with CoolingSystemConsumption = pvalue } } }
    | "Electrolyzers.GasManagementConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with GasManagementConsumption = pvalue } } }
    | "Electrolyzers.OverhaulElectrolyzer" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with OverhaulElectrolyzer = pvalue } } }
    | "Electrolyzers.H2CompressorSpecificConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with H2CompressorSpecificConsumption = Some pvalue } } }
    | "Electrolyzers.OxigenCompressorConsumption" ->
        { inp with
            SystemInputs =
                { inp.SystemInputs with Electrolyzers = { inp.SystemInputs.Electrolyzers with OxigenCompressorConsumption = pvalue } } }

    | _ -> inp
