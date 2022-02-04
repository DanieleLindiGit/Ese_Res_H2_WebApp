module TestRunner

type ProjectInput = {
    SystemInputs: Inputs.SystemInputs
    FinancialInputs: EconomicInputs.FinancialInputs
}

type IntOrFloat =
    | IntValue of int
    | FloatValue of float