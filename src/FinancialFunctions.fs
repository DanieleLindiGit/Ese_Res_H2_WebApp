module FinancialFunctions

let NPV (discountRate: float) (cashFlow: float list) =
   let getFactor (idx: int) (value: float) =
      value / ( (1.0 + discountRate / 100.0) ** ( float idx + 1.0) )
   cashFlow |> List.mapi getFactor |> List.sum

let IRR (cashFlow: float list) =
   let candidates = [-100.0 .. 1.0 .. 100.0] 
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
         |> List.map (fun e -> (e, abs(NPV e cashFlow) ))
         |> List.sortBy snd
         |> List.take 2

      let value1 = snd indexCloseToZero.[0]
      let value2 = snd indexCloseToZero.[1]
      let limit1 = fst indexCloseToZero.[0]
      let limit2 = fst indexCloseToZero.[1] 
 
      match (value1, value2) with
      | (a, _) when a < 0.01 -> limit1
      | (_, b) when b < 0.01 -> limit2
      | _ -> IrrIterator (limit1, limit2)

   IrrIterator (0.0, 100.0)