module Validation

open Browser.Dom

// class is-valid or is-invalid
// cases are
// integer
// float
// float list
// float max50
// float option
// no-validation

type ValidationCase = 
   | Integer = 0 // integer
   | Float = 1 // float
   | FloatList = 2 // float list
   | FloatMax50 = 3 // float max50
   | FloatOption = 4 // float option
   | NoValidation = 10 // no-validation

let ValidateSingleInput = 0

let ValidateAllInputs () =
  let elements = document.getElementsByClassName("form-control")
  [0 .. elements.length-1]
  |> List.iter (fun idx -> 
      let l = elements.[idx]
      l.classList.remove("is-valid")
      l.classList.remove("is-invalid")
      let value = 
        match l.tagName with
        | "TEXTAREA" -> (l :?> Browser.Types.HTMLTextAreaElement).value
        | _ -> (l :?> Browser.Types.HTMLInputElement).value
      let cl = l.getAttribute("class").Replace("form-control", "").Trim()
      ()
      )
  0