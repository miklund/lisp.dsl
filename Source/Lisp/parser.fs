// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 ".\Source\Lisp\parser.fsy"

open System
open Microsoft.FSharp.Collections
open Lisp.Statements


# 13 ".\Source\Lisp\parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | END
  | LPAREN
  | RPAREN
  | DEFUN
  | IDENTIFIER of (string)
  | BOOLEAN of (bool)
  | NUMBER of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_END
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_DEFUN
    | TOKEN_IDENTIFIER
    | TOKEN_BOOLEAN
    | TOKEN_NUMBER
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_parameters
    | NONTERM_arguments

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | END  -> 0 
  | LPAREN  -> 1 
  | RPAREN  -> 2 
  | DEFUN  -> 3 
  | IDENTIFIER _ -> 4 
  | BOOLEAN _ -> 5 
  | NUMBER _ -> 6 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_END 
  | 1 -> TOKEN_LPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_DEFUN 
  | 4 -> TOKEN_IDENTIFIER 
  | 5 -> TOKEN_BOOLEAN 
  | 6 -> TOKEN_NUMBER 
  | 9 -> TOKEN_end_of_input
  | 7 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_expression 
    | 3 -> NONTERM_expression 
    | 4 -> NONTERM_expression 
    | 5 -> NONTERM_expression 
    | 6 -> NONTERM_expression 
    | 7 -> NONTERM_parameters 
    | 8 -> NONTERM_parameters 
    | 9 -> NONTERM_arguments 
    | 10 -> NONTERM_arguments 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 9 
let _fsyacc_tagOfErrorTerminal = 7

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | END  -> "END" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | DEFUN  -> "DEFUN" 
  | IDENTIFIER _ -> "IDENTIFIER" 
  | BOOLEAN _ -> "BOOLEAN" 
  | NUMBER _ -> "NUMBER" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | END  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | DEFUN  -> (null : System.Object) 
  | IDENTIFIER _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BOOLEAN _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUMBER _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 4us; 65535us; 0us; 2us; 11us; 12us; 14us; 17us; 17us; 17us; 2us; 65535us; 14us; 15us; 17us; 18us; 2us; 65535us; 10us; 11us; 20us; 21us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 8us; 11us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 3us; 1us; 4us; 2us; 5us; 6us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 10us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 17us; 19us; 21us; 23us; 25us; 27us; 29us; 31us; 33us; 35us; 37us; 39us; 41us; 43us; |]
let _fsyacc_action_rows = 22
let _fsyacc_actionTableElements = [|4us; 32768us; 1us; 7us; 4us; 6us; 5us; 5us; 6us; 4us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 0us; 16388us; 2us; 32768us; 3us; 8us; 4us; 14us; 1us; 32768us; 4us; 9us; 1us; 32768us; 1us; 10us; 2us; 32768us; 2us; 19us; 4us; 20us; 4us; 32768us; 1us; 7us; 4us; 6us; 5us; 5us; 6us; 4us; 1us; 32768us; 2us; 13us; 0us; 16389us; 5us; 32768us; 1us; 7us; 2us; 16us; 4us; 6us; 5us; 5us; 6us; 4us; 0us; 16390us; 0us; 16391us; 5us; 32768us; 1us; 7us; 2us; 16us; 4us; 6us; 5us; 5us; 6us; 4us; 0us; 16392us; 0us; 16393us; 2us; 32768us; 2us; 19us; 4us; 20us; 0us; 16394us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 8us; 9us; 10us; 11us; 12us; 15us; 17us; 19us; 22us; 27us; 29us; 30us; 36us; 37us; 38us; 44us; 45us; 46us; 49us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 1us; 7us; 3us; 1us; 2us; 1us; 2us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16386us; 16387us; 16388us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16389us; 65535us; 16390us; 16391us; 65535us; 16392us; 16393us; 65535us; 16394us; |]
let _fsyacc_reductions ()  =    [| 
# 118 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Lisp.Statements.Ast)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 127 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 ".\Source\Lisp\parser.fsy"
                                       _1 
                   )
# 21 ".\Source\Lisp\parser.fsy"
                 : Lisp.Statements.Ast));
# 138 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 ".\Source\Lisp\parser.fsy"
                                 Number(_1) 
                   )
# 24 ".\Source\Lisp\parser.fsy"
                 : 'expression));
# 149 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 ".\Source\Lisp\parser.fsy"
                                  Boolean(_1) 
                   )
# 25 ".\Source\Lisp\parser.fsy"
                 : 'expression));
# 160 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 ".\Source\Lisp\parser.fsy"
                                     Identifier(_1) 
                   )
# 26 ".\Source\Lisp\parser.fsy"
                 : 'expression));
# 171 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : 'arguments)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 ".\Source\Lisp\parser.fsy"
                                                                                     Defun(_3, _5, _6) 
                   )
# 27 ".\Source\Lisp\parser.fsy"
                 : 'expression));
# 184 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'parameters)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 ".\Source\Lisp\parser.fsy"
                                                       Call(_2, _3) 
                   )
# 28 ".\Source\Lisp\parser.fsy"
                 : 'expression));
# 196 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 ".\Source\Lisp\parser.fsy"
                                 [] 
                   )
# 31 ".\Source\Lisp\parser.fsy"
                 : 'parameters));
# 206 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expression)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'parameters)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 ".\Source\Lisp\parser.fsy"
                                                _1 :: _2 
                   )
# 32 ".\Source\Lisp\parser.fsy"
                 : 'parameters));
# 218 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 ".\Source\Lisp\parser.fsy"
                                 [] 
                   )
# 35 ".\Source\Lisp\parser.fsy"
                 : 'arguments));
# 228 ".\Source\Lisp\parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'arguments)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 ".\Source\Lisp\parser.fsy"
                                               Identifier(_1) :: _2 
                   )
# 36 ".\Source\Lisp\parser.fsy"
                 : 'arguments));
|]
# 241 ".\Source\Lisp\parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 10;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : Lisp.Statements.Ast =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
