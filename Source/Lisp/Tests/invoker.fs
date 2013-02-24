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

// function call
[<Fact>]
let ``should invoke (add 1 2) as 3`` () =
    "(add 1 2)" |> invoke<int> |> should equal 3 // ironicly this is also valid fsharp code

[<Fact>]
let ``should invoke (sub 4 3) as 1`` () =
    "(sub 4 3)" |> invoke<int> |> should equal 1

[<Fact>]
let ``should invoke (add (add 1 2) (add 3 (add 4 5))) as 15`` () =
    "(add (add 1 2) (add 3 (add 4 5)))" |> invoke<int> |> should equal 15

[<Fact>]
let ``should invoke (eq 1 2)`` () =
    "(eq 1 2)" |> invoke<bool> |> should equal false

[<Fact>]
let ``should invoke (if (eq 1 2) (add 1 2) (sub 1 2)) as -1`` () =
    "(if (eq 1 2) (add 1 2) (sub 1 2))" |> invoke<int> |> should equal -1

// defun
[<Fact>]
let ``should be able to call a defined function`` () =
    @"(defun myAdd (x y) (add x y))
      (myAdd 1 2)" |> invoke<int> |> should equal 3

[<Fact>]
let ``don't mix up the order of the arguments`` () =
    @"(defun mySub (x y) (sub x y))
      (mySub 5 3)" |> invoke<int> |> should equal 2

[<Fact>]
let ``nesting function definitions is no problem`` () =
    @"(defun addFive (x) (add x 5))
      (defun subThree (x) (sub x 3))
      (subThree (addFive 10))" |> invoke<int> |> should equal 12

[<Fact(Skip = "Not implemented in invoker")>]
let ``recursion should be allowed`` () =
    @"(defun mult (x y)
         (if (lt y 1) 0 (mult x (sub y 1))))
      (mult 4 5)" |> invoke<int> |> should equal 20

// expressions
[<Fact>]
let ``should only invoke first expression on primitive types and function calls`` () =
    "1\n2" |> invoke<int> |> should equal 1
