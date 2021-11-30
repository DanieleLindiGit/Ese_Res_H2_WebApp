module FinancialFunctions

let NPV (discountRate: float) (cashFlow: float list) =
   let getFactor (idx: int) (value: float) =
      value / ( (1.0 + discountRate / 100.0) ** ( float idx + 1.0) )
   cashFlow |> List.mapi getFactor |> List.sum

//TODO: Find exact list if candidates
let IRR (cashFlow: float list) =
   let candidates = [0.0 .. 1.0 .. 100.0] 
   let indexCloseToZero =
      candidates
      |> List.map (fun e -> NPV e cashFlow)
      |> List.map abs
      |> List.indexed //trasforma in una tupla indice * valore
      |> List.minBy snd
      |> fst
   candidates.[indexCloseToZero]

let IRR2 (cashFlow: float list) =
   let rec IrrIterator (limits: float * float) =
      let v1, v2 = limits
      let lowerLimit = min v1 v2
      let upperLimit = max v1 v2
      let step = (upperLimit - lowerLimit) / 10.0
      let candidates = [lowerLimit .. step .. upperLimit] 
      let indexCloseToZero =
         candidates
         |> List.map (fun e -> NPV e cashFlow)
         |> List.map abs
         |> List.indexed //trasforma in una tupla indice * valore
         |> List.sortBy snd
         |> List.map fst
   
      let low = List.item (indexCloseToZero.[1]) candidates
      let up = List.item (indexCloseToZero.[0]) candidates
      let diff = abs (up-low)
 
      match diff with
      |  v when v < 0.01 -> up
      | _ -> IrrIterator (low, up)

   IrrIterator (0.0, 100.0)