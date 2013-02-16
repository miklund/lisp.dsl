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