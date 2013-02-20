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
