module Invoker

open Lisp.Statements
open Microsoft.FSharp.Quotations
open Linq.QuotationEvaluation
open Microsoft.FSharp.Reflection

// functions that are callable from inside my lisp program
let framework = 
    [
        "add", typeof<int -> int -> int>, <@@ (fun a b -> a + b) @@>;
        "sub", typeof<int -> int -> int>, <@@ (fun a b -> a - b) @@>;
        "eq", typeof<int -> int -> bool>, <@@ (fun (a : int) b -> a = b) @@>;
        "if", typeof<bool -> int -> int -> int>, <@@ (fun cond (yes : int) no -> if cond then yes else no) @@>
    ]

// Create an application
// Example: Application (Application (add, Value (1)), Value (2))
let application var args =
    args |> List.fold(fun prev next -> Quotations.Expr.Application(prev, next)) var

// convert ast to Quotations.Expr
let rec toExprUntyped vars = function
| Number(x)  -> Quotations.Expr.Value(x)
| Boolean(x) -> Quotations.Expr.Value(x)
| Call(name, arguments) ->
    // resolve arguments
    let argumentExpressions = arguments |> List.map (fun arg -> (toExprUntyped vars arg))
    // create application
    application (vars |> Map.find(name)) argumentExpressions

// typed version of toExprUntyped
let toExpr<'a> (vars : Map<string, Quotations.Expr>) ast : Quotations.Expr<'a> =
    (toExprUntyped vars ast) |> Quotations.Expr<'a>.Cast

// take name, function signature and body, create a var and let expression in a tuple
let create_fn (name, signature, lambda) =
    let var = Quotations.Var(name, signature)
    var, (fun (body : Quotations.Expr) -> Quotations.Expr.Let(var, lambda, body))

// make next let expression become body of the previous
let rec mergeLetExpressions<'a> (exprs : (Quotations.Expr -> Quotations.Expr) list) (body : Quotations.Expr<'a>) = 
    match exprs with
    | [] -> body
    | hd :: tl -> hd(mergeLetExpressions tl body) |> Quotations.Expr<'a>.Cast

// execute ast
let invoke<'a> (framework : (string * System.Type * Quotations.Expr) list) (ast : Ast list) = 
    let vars, exprs = framework |> List.map create_fn |> List.unzip
    // create vars map
    let state = vars |> List.map (fun var -> var.Name, Quotations.Expr.Var(var)) |> Map.ofList
    // build expression tree from framework
    let header = (mergeLetExpressions<'a> exprs)
    // join framework expression tree with intepretated ast
    (header (toExpr<'a> state (ast.Head))).Eval()