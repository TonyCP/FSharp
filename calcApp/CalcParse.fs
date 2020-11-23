// CalcParse.fs //
module CalcParse

// Convert string representation of numner to double float point //
let parseFloat (s : string) =
    match System.Double.TryParse(s) with
    | true, z -> Some z
    | false, _ -> None
