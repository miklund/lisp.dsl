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