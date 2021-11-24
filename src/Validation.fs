module Validation

open Browser.Dom
open DomHelpers

// class is-valid or is-invalid
// cases are
// integer
// float
// float list
// float max50
// float option
// no-validation

type ValidationElement = {
  DomElement: Browser.Types.Element
  Value: string
  IsValid: bool
}

type ValidationCase = 
   | Integer = 0 // integer
   | Float = 1 // float
   | FloatList = 2 // float list
   | FloatMax50 = 3 // float max50
   | FloatOption = 4 // float option
   | NoValidation = 10 // no-validation

let ShowErrorMessage () =
   document.getElementById("alertDiv").innerHTML <- 
      """
      <div class="alert alert-danger" role="alert">
         <strong>Some inputs are not valid!</strong> You should check in on some of those fields with errors below.
      </div> 
      """
   document.getElementById("InputsDiv").scrollIntoView()

let ValidateSingleInput (element:Browser.Types.Element) 
  (value: string) 
  (vc: ValidationCase) =
  let intChars = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
  let floatChars = ',' :: intChars
  let floatListChars = floatChars @ [';'; ' '; '\u000A'] // 000A Newline in unicode
  let v2 = Seq.toList value
  let allCharsAreValid (list: char list) = v2 |> List.forall (fun c -> List.contains c list)
   
  let isValid = 
    match vc with
    | ValidationCase.Integer -> allCharsAreValid intChars
    | ValidationCase.Float -> allCharsAreValid floatChars
    | ValidationCase.FloatList -> allCharsAreValid floatListChars
    | ValidationCase.FloatMax50 -> allCharsAreValid floatChars && parseFloat value <= 50.0
    | ValidationCase.FloatOption -> value.Length = 0 || allCharsAreValid floatChars
    | _ -> true
  
  if not isValid then element.classList.add("is-invalid")

  {ValidationElement.DomElement = element; Value = value; IsValid = isValid}
  

let ValidateAllInputs () =
  document.getElementById("alertDiv").innerHTML <- ""
  let elements = document.getElementsByClassName("form-control")
  let validationsElements =
    [0 .. elements.length-1]
    |> List.map (fun idx -> 
      let l = elements.[idx]
      l.classList.remove("is-valid")
      l.classList.remove("is-invalid")
      let value = 
        match l.tagName with
        | "TEXTAREA" -> (l :?> Browser.Types.HTMLTextAreaElement).value
        | _ -> (l :?> Browser.Types.HTMLInputElement).value
      let cl = l.getAttribute("class").Replace("form-control", "").Trim()
      let vc =
        match cl with
        | "integer" -> ValidationCase.Integer
        | "float"    -> ValidationCase.Float
        | "float list" -> ValidationCase.FloatList
        | "float max50" -> ValidationCase.FloatMax50
        | "float option" -> ValidationCase.FloatOption
        | _ -> ValidationCase.NoValidation

      ValidateSingleInput l value vc)

  validationsElements |> List.forall (fun e -> e.IsValid)