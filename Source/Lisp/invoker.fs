module Invoker

open Lisp.Statements
open Microsoft.FSharp.Quotations
open Linq.QuotationEvaluation

let framework = 
    [
        "add", <@@ let add a b = a + b in add @@>
    ] |> Map.ofList

// convert ast to Quotations.Expr
let toExpr<'a> (state : Map<string, Quotations.Expr>) ast : Quotations.Expr<'a> =
    match ast with
    | Number(x)  -> <@ x @> |> Quotations.Expr<'a>.Cast
    | Boolean(x) -> <@ x @> |> Quotations.Expr<'a>.Cast

// execute ast
let invoke<'a> (state : Map<string, Quotations.Expr>) ast = (toExpr<'a> state ast).Eval()