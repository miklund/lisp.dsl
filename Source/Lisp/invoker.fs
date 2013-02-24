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
| [] -> failwith "Nothing to invoke"
| Number(x) :: tl -> Quotations.Expr.Value(x)
| Boolean(x) :: tl -> Quotations.Expr.Value(x)
| Identifier(x) :: tl -> vars |> Map.find(x)
| Call(name, arguments) :: tl ->
    // resolve arguments
    let argumentExpressions = arguments |> List.map (fun arg -> (toExprUntyped vars [arg]))
    // create application
    application (vars |> Map.find(name)) argumentExpressions
// debug values
// let name = "myAdd"
// let parameters = [("x", typeof<int>); ("y", typeof<int>)]
// let bodyAst = Call ("add", [(Identifier "x"); (Identifier "y")])
| Defun(name, parameters, bodyAst) :: tl ->
    // create function local variables
    let localVars = parameters |> List.map (fun (paramName, paramType) -> Quotations.Var(paramName, paramType))
    // create function local variables expressions
    let localVarsExpr = localVars |> List.map (Quotations.Expr.Var)
    // create local scope
    let localScope = List.zip parameters localVarsExpr |> List.fold (fun scope ((paramName, _), varExpr) -> scope |> Map.add paramName varExpr) vars
    // evaluate body
    let bodyExpr = toExprUntyped localScope [bodyAst]
    // create body lambda
    let lambdaExpr = localVars |> List.rev |> List.fold (fun expr var -> Quotations.Expr.Lambda(var, expr)) bodyExpr
    // create function handle
    let funcVar = Quotations.Var(name, lambdaExpr.Type)
    // create let expression
    let letExpr next = Quotations.Expr.Let(funcVar, lambdaExpr, next)
    // return evaluation of next, with this function in scope
    letExpr (toExprUntyped (vars.Add(name, Quotations.Expr.Var(funcVar))) tl)

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
    let state, exprs = framework |> List.map create_fn |> List.unzip
    // create vars map
    let vars = state |> List.map (fun var -> var.Name, Quotations.Expr.Var(var)) |> Map.ofList
    // build expression tree from framework
    let header = (mergeLetExpressions<'a> exprs)
    // join framework expression tree with intepretated ast
    (header (toExpr<'a> vars (ast))).Eval()