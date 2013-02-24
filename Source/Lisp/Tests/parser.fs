module tests.parser

open Xunit
open FsUnit.Xunit
open Lisp.Statements
open tests.lexer

// help method
let parse = lex >> (Parser.start (Lexer.tokenize 0))

// number
[<Fact>]
let ``empty is not a program`` () =
    (fun () -> System.String.Empty |> parse |> ignore) |> should throw typeof<System.Exception>

[<Fact>]
let ``digits are parsed as number`` () =
    "-11" |> parse |> should equal (Number -11)

// boolean
[<Fact>]
let ``t is parsed as boolean(true)`` () =
    "t" |> parse |> should equal (Boolean true)

[<Fact>]
let ``nil is parsed as boolean(false)`` () =
    "nil" |> parse |> should equal (Boolean false)

// call
[<Fact>]
let ``(add 1 2) is parsed as call with parameters`` () =
    "(add 1 2)" |> parse |> should equal (Call ("add", [Number 1; Number 2]))

[<Fact>]
let ``(add (add 1 2) 3) is parsed as nested function alls`` () =
    "(add (add 1 2) 3)" |> parse |> should equal (Call ("add", [Call ("add", [Number 1; Number 2]); Number 3]))

// defun
[<Fact>]
let ``(defun one () 1) is parsed as a function definition`` () =
    "(defun one () 1)" |> parse |> should equal (Defun ("one", [], Number 1))

[<Fact>]
let ``(defun addTwo (x) (add x 2)) is parsed as a function definition`` () =
    "(defun addTwo (x) (add x 2))" |> parse |> should equal (Defun ("addTwo", [Identifier "x"], (Call ("add", [Identifier "x"; Number 2]))))
    |> should equal (Defun ("addTwo", [("x", typeof<int>)], (Call ("add", [Identifier "x"; Number 2])), Call ("addTwo", [Number 3])))

[<Fact>]
let ``(defun myAdd (x y) (add x y)) (myAdd 3 4) is parsed as a function definition`` () =
    "(defun myAdd (x y) (add x y))" |> parse |> should equal (Defun ("myAdd", [Identifier "x"; Identifier "y"], (Call ("add", [Identifier "x"; Identifier "y"]))))
    |> should equal (Defun ("myAdd", [("x", typeof<int>); ("y", typeof<int>)], (Call ("add", [Identifier "x"; Identifier "y"])), Call ("myAdd", [Number 3; Number 4])))
let ``should place function definitions after each other`` () =
    @"(defun one () 1) (one)" |> parse |> should equal (Defun ("one", [], Number 1, Call ("one", [])))

[<Fact>]
let ``should place function definitions in each own rows`` () =
    @"(defun addFive (x) (add x 5))
      (defun subThree (x) (sub x 3))
      1" 
      |> parse 
      |> should equal (Defun ("addFive", ["x", typeof<int>], Call ("add", [Identifier "x"; Number 5]),
                         Defun ("subThree", ["x", typeof<int>], Call ("sub", [Identifier "x"; Number 3]),
                           Number 1)))
