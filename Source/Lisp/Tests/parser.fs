module tests.parser

open Xunit
open FsUnit.Xunit
open Lisp.Statements
open tests.lexer

// help method
let parse = lex >> (Parser.start (Lexer.tokenize 0))

[<Fact>]
let ``empty is not a program`` () =
    System.String.Empty |> parse |> should equal Unparsed