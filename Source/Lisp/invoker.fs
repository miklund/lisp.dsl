module Invoker

open Lisp.Statements
open Microsoft.FSharp.Quotations
open Linq.QuotationEvaluation
open Microsoft.FSharp.Reflection

let framework = 
    [
        "add", typeof<int -> int -> int>, <@@ (fun a b -> a + b) @@>;
        "sub", typeof<int -> int -> int>, <@@ (fun a b -> a - b) @@>;
        "eq", typeof<int -> int -> bool>, <@@ (fun (a : int) b -> a = b) @@>;
        "if", typeof<bool -> int -> int -> int>, <@@ (fun cond (yes : int) no -> if cond then yes else no) @@>
    ]

// success
//let var = Quotations.Var("add", typeof<int -> int>)
//let expr = Quotations.Expr.Var(var)
//Quotations.Expr.Let(var, <@@ (fun a -> a + 2) @@>, (Quotations.Expr.Application(expr, <@@ 3 @@>))).Eval()
// success

let rec expand (functionType : System.Type) =
    if FSharpType.IsFunction(functionType) then
        let first, rest = FSharpType.GetFunctionElements(functionType)
        first :: expand(rest)
    else
        []

let rec application var = function
| [expr]   -> Quotations.Expr.Application(var, expr)
| hd :: tl -> Quotations.Expr.Application((application var tl), hd)
| []       -> failwith "Functions with no arguments aren't pure functions"


//let rec toExpr vars = function
//| Number(x)  -> Quotations.Expr.Value(x)
//| Boolean(x) -> Quotations.Expr.Value(x)
//| Call(name, arguments) ->
//    // resolve arguments
//    let argumentExpressions = arguments |> List.map (fun arg -> <@@ %%(toExpr vars arg) @@>)
//    // create application
//    application (vars |> Map.find(name)) (argumentExpressions |> List.rev)


//<@@ let addTwo a = a + 2 in addTwo 3 @@>
// convert ast to Quotations.Expr
type Expressionist =
    static member toExprUntyped t =
        (fun (vars : Map<string, Quotations.Expr>) (ast : Ast) -> 
            let definition = (typeof<Expressionist>.GetMethod "toExpr").MakeGenericMethod [| t |]
            definition.Invoke(null, [|vars; ast|]) :?> Quotations.Expr
        )

    static member toExpr<'a> (vars : Map<string, Quotations.Expr>) ast : Quotations.Expr<'a> =
        printfn "%A\n" ast
        match ast with
        | Number(x)  -> <@ x @> |> Quotations.Expr<'a>.Cast
        | Boolean(x) -> <@ x @> |> Quotations.Expr<'a>.Cast
        | Call(name, arguments) ->
            // function reference
            let func = vars |> Map.find(name)
            // resolve arguments
            let argumentExpressions = arguments |> List.zip (expand (func.Type)) |> List.map (fun (t, arg) -> <@@ %%((Expressionist.toExprUntyped t) vars arg) @@>)
            // create application
            application func (argumentExpressions |> List.rev) |> Quotations.Expr<'a>.Cast

let create_fn (name, signature, lambda) =
    let var = Quotations.Var(name, signature)
    var , (fun (body : Quotations.Expr) -> Quotations.Expr.Let(var, lambda, body))

let rec mergeLetExpressions<'a> (exprs : (Quotations.Expr -> Quotations.Expr) list) (body : Quotations.Expr<'a>) = 
    match exprs with
    | [] -> body
    | hd :: tl -> hd(mergeLetExpressions tl body) |> Quotations.Expr<'a>.Cast

// execute ast
// let ast = Number 1
let invoke<'a> (framework : (string * System.Type * Quotations.Expr) list) ast = 
    let vars, exprs = framework |> List.map create_fn |> List.unzip
    // create vars map
    let state = vars |> List.map (fun var -> var.Name, Quotations.Expr.Var(var)) |> Map.ofList
    // build expression tree from framework
    let header = (mergeLetExpressions<'a> exprs)
    // join framework expression tree with intepretated ast
    (header (Expressionist.toExpr<'a> state ast)).Eval()