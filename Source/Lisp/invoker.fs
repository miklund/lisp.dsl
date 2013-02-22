module Invoker

open Lisp.Statements
open Microsoft.FSharp.Quotations
open Linq.QuotationEvaluation

let framework = 
    [
        "add", typeof<int -> int -> int>, <@@ (fun a b -> a + b) @@>;
        "sub", typeof<int -> int -> int>, <@@ (fun a b -> a - b) @@>
    ]

// success
//let var = Quotations.Var("add", typeof<int -> int>)
//let expr = Quotations.Expr.Var(var)
//Quotations.Expr.Let(var, <@@ (fun a -> a + 2) @@>, (Quotations.Expr.Application(expr, <@@ 3 @@>))).Eval()
// success

let rec application var = function
| [expr]   -> Quotations.Expr.Application(var, expr)
| hd :: tl -> Quotations.Expr.Application((application var tl), hd)
| []       -> failwith "Functions with no arguments aren't pure functions"

//<@@ let addTwo a = a + 2 in addTwo 3 @@>
// convert ast to Quotations.Expr
let rec toExpr<'a> vars ast : Quotations.Expr<'a> =
    match ast with
    | Number(x)  -> <@ x @> |> Quotations.Expr<'a>.Cast
    | Boolean(x) -> <@ x @> |> Quotations.Expr<'a>.Cast
    | Call(name, arguments) ->
        // resolve arguments
        let argumentExpressions = arguments |> List.map (fun arg -> <@ %(toExpr vars arg) @>)
        // create application
        application (vars |> Map.find(name)) (argumentExpressions |> List.rev) |> Quotations.Expr<'a>.Cast

let create_fn (name, signature, lambda) =
    let var = Quotations.Var(name, signature)
    var , (fun (body : Quotations.Expr) -> Quotations.Expr.Let(var, lambda, body))

let rec mergeLetExpressions<'a> (exprs : (Quotations.Expr -> Quotations.Expr) list) (body : Quotations.Expr<'a>) = 
    match exprs with
    | [] -> body
    | hd :: tl -> hd(mergeLetExpressions tl body) |> Quotations.Expr<'a>.Cast

// (mergeLetExpressions <@ (add 1 2) @> exprs).Eval()
// (mergeLetExpressions (toExpr<int> state ast) exprs).Eval()

// let ast = Number 1

// invoke<int> framework (Number 1)

// execute ast
let invoke<'a> (framework : (string * System.Type * Quotations.Expr) list) ast = 
    let vars, exprs = framework |> List.map create_fn |> List.unzip
    // create vars map
    let state = vars |> List.map (fun var -> var.Name, Quotations.Expr.Var(var)) |> Map.ofList
    // build expression tree from framework
    let header = (mergeLetExpressions<'a> exprs)
    // join framework expression tree with intepretated ast
    (header (toExpr<'a> state ast)).Eval()