module TestRunner

(* type ProjectInput = {
    SystemInputs: Inputs.SystemInputs
    FinancialInputs: EconomicInputs.FinancialInputs
} *)

// Restituisce le possibile combinazioni in un vettore di matrici
// ESEMPIO: CartesianProduct [[1;2];[3;4;5];[6;7]] |> Seq.toList
// RETURNS: [[1; 3; 6]; [1; 3; 7]; [1; 4; 6]; [1; 4; 7]; [1; 5; 6]; 
//           [1; 5; 7]; [2; 3; 6]; [2; 3; 7]; [2; 4; 6]; [2; 4; 7];
//           [2; 5; 6]; [2; 5; 7]]
let rec CartesianProduct LL = 
    match LL with
    | [] -> Seq.singleton []
    | L::Ls -> seq {for x in L do for xs in CartesianProduct Ls -> x::xs}


type Parameter = {
    ParameterName: string
    Values: float list
}

let ParseFloatValue (v: string) =
    float (v.Replace(".", System.String.Empty).Replace(",", "."))

let SplitByChar (v: string)  (sep: string) =
    v.Split([| sep |], System.StringSplitOptions.RemoveEmptyEntries) 
    |> List.ofArray

let SplitRange v = SplitByChar v ".."

let SplitNameAndValues v =
    let tuple = SplitByChar v "="
    (tuple.[0].Trim(), tuple.[1])

let GetFloatRange v = 
   let vs = SplitRange v |> List.map ParseFloatValue
   [vs.[0] .. vs.[1] .. vs.[2]]

let GetParameterFromString v =
   let (name, s_values) = SplitNameAndValues v
   let values = GetFloatRange s_values
   {
       ParameterName = name
       Values = values
   }
