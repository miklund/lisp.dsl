module tests.lexer

open Xunit
open FsUnit.Xunit
open Lisp.Statements

// help method
let lex s = Lexing.LexBuffer<_>.FromString s

// toString
let toString (buf : Lexing.LexBuffer<char>) = 
    let join separator (collection : string seq) = System.String.Join(separator, collection)
    seq {
        while not buf.IsPastEndOfStream do
            yield (Lexer.tokenize 0 buf)
    } 
    |> Seq.map Parser.token_to_string
    |> join " "
    

[<Fact>]
let ``empty is not a program`` () =
    System.String.Empty |> lex |> toString |> should equal "END"

// Number
[<Fact>]
let ``single digit number is a program`` () =
    "1" |> lex |> toString |> should equal "NUMBER END"

[<Fact>]
let ``double digit number is one number and end`` () =
    "11" |> lex |> toString |> should equal "NUMBER END"

[<Fact>]
let ``negative number is lexed as a number`` () =
    "-11" |> lex |> toString |> should equal "NUMBER END"

// boolean
[<Fact>]
let ``t is lexed as a boolean`` () =
    "t" |> lex |> toString |> should equal "BOOLEAN END"

[<Fact>]
let ``nil is lexed as boolean`` () =
    "nil" |> lex |> toString |> should equal "BOOLEAN END"

// calls
[<Fact>]
let ``(add 1 2) is parsed as a function call`` () =
    "(add 1 2)" |> lex |> toString |> should equal "LPAREN IDENTIFIER NUMBER NUMBER RPAREN END"

[<Fact>]
let ``(add (add 1 2) 3) is parsed as nested call`` () =
    "(add (add 1 2) 3)" |> lex |> toString |> should equal "LPAREN IDENTIFIER LPAREN IDENTIFIER NUMBER NUMBER RPAREN NUMBER RPAREN END"

// defun
[<Fact>]
let ``(defun addTwo (x) (add x 2)) is parsed as function definition`` () =
    "(defun addTwo (x) (add x 2))" |> lex |> toString |> should equal "LPAREN DEFUN IDENTIFIER LPAREN IDENTIFIER RPAREN LPAREN IDENTIFIER IDENTIFIER NUMBER RPAREN RPAREN END"

[<Fact>]
let ``(defun myAdd (x y) (add x y)) is parsed as function definition`` () =
    "(defun myAdd (x y) (add x y))" |> lex |> toString |> should equal "LPAREN DEFUN IDENTIFIER LPAREN IDENTIFIER IDENTIFIER RPAREN LPAREN IDENTIFIER IDENTIFIER IDENTIFIER RPAREN RPAREN END"
