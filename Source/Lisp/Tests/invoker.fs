module tests.intepreter

open Xunit
open FsUnit.Xunit

open tests.parser

// help method
let invoke<'a> = parse >> (Invoker.invoke<'a> Invoker.framework)

// number
[<Fact>]
let ``should invoke "-11" as -11`` () =
    "-11" |> invoke<int> |> should equal -11

// boolean
[<Fact>]
let ``should invoke "t" as true`` () =
    "t" |> invoke<bool> |> should equal true

[<Fact>]
let ``should invoke "nil" as false`` () =
    "nil" |> invoke<bool> |> should equal false
