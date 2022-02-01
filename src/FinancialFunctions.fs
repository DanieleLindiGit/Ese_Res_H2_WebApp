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

let PMT (interest:float) (nbr_of_periods:int) (amount:float) =
   let pvif = (1.0 + interest) ** (float nbr_of_periods)
   interest / (pvif - 1.0) * -(amount * pvif)

let IPMT (amount: float) (pmt:float) (interest: float) (year: int) =
   let tmp = (1.0 + interest) ** (float year - 1.0)
   0.0 - (amount * tmp * interest + pmt * (tmp - 1.0))

let Interessi (interest:float) (year:int) (nbr_of_periods: int) (amount: float) =
   let pmt = PMT interest nbr_of_periods amount
   IPMT amount pmt interest year

let PrestitoRata (interest:float) (year:int) (nbr_of_periods: int) (amount: float) =
   let pmt = PMT interest nbr_of_periods amount
   let impt = IPMT amount pmt interest year
   pmt - impt

