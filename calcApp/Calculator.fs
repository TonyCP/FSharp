//* Calculator.fs *//
module Calculator
open System
exception DivideByZeroException of string


// Mathematic operators //
type Operator = 
    | Addition
    | Subtraction
    | Multiplication
    | Division

// Input for calculation //
type CState = 
    | ExpectingX
    | ExpectingOp of float
    | ExpectingY of float * Operator 
    | Complete

// Retreive operator input //
let getOperator z =
    match z with
    | "+" -> Some Addition
    | "-" -> Some Subtraction
    | "*" -> Some Multiplication
    | "/" -> Some Division
    | _   -> None

// Retrieve first num input //
let numPrompt1 = 
    printfn "Provide input number: "
    let input = Console.ReadLine()
    let inpToFloat = CalcParse.parseFloat input
    match inpToFloat with 
    | Some(n) -> ExpectingOp n
    | None    -> Complete

// Retrieve operator input //
let opPrompt z = 
    printfn "Provide one of following operators: +, -, *, /"
    let input = Console.ReadLine()
    let inpToOperator = getOperator input
    match inpToOperator with
    | None -> Complete
    | Some(op) -> ExpectingY(z, op)

// Set up input calculation //
let calculate z (op: Operator) y = 
    match op with
    | Addition -> (z + y)
    | Subtraction -> (z - y)
    | Multiplication -> (z * y)
    | Division -> match y with
                  | 0.0 -> raise ( DivideByZeroException "No division by zero!")
                  | _ -> (z / y)    

// Format and implement calculation //
let finalCalculate z op y =
    let answer = 
        try
            calculate z op y
        with
            | DivideByZeroException(str) -> printfn "Error: %s. Reseting.." str; 0.0

    printfn "Final Calculation: %A" answer
    ExpectingOp answer
 

// Retrieve second num input //
let numPrompt2 z op =
    printfn "Provide input number: "
    let input = Console.ReadLine()
    let inpToFloat = CalcParse.parseFloat input
    match inpToFloat with
    | Some(n) -> finalCalculate z op n
    | None -> Complete


    
// Progress in calculation //
let nextState state =
    match state with
    | ExpectingX -> numPrompt1
    | ExpectingOp(z) -> opPrompt z
    | ExpectingY(z, op) -> numPrompt2 z op
    | Complete -> Complete

// Math program loop //
let beginLoop = fun () ->
    let inputSeq = Seq.unfold (fun state ->
        match state with
        | Complete -> None
        | incompleteState ->
            let newState = nextState incompleteState
            Some (newState, newState)) ExpectingX

    for _ in inputSeq do ()

// Begin loop and print to user //
let startCalc = fun () ->
    printfn "Begin calculation.."
    beginLoop()
    printfn "Invalid input, exiting..."
    0

