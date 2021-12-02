module FinancialFunctions

//si suppone che il primo elemento sia l'investimento [-x] ed il resto degli incassi [+x]
let Gain (cashFlow: float list) = (List.sum cashFlow) / cashFlow.Head * -1.0
   
let NPV (discountRate: float) (cashFlow: float list) =
   let getFactor (idx: int) (value: float) =
      value / ( (1.0 + discountRate / 100.0) ** ( float idx + 1.0) )
   cashFlow |> List.mapi getFactor |> List.sum

// restituisce un valore compreso tra -100% e +100% con risoluzione a 0.1%
let IRR (cashFlow: float list) =
   let candidates = 
      match Gain cashFlow with
      | v when v > 0.0 -> [0.0 .. 0.1 .. 100.0]
      | _ -> [0.0 .. -0.1 .. -100.0] 
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
      // controllo dello step a 0
      match step with
      | 0.0 -> lowerLimit
      | _ ->

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