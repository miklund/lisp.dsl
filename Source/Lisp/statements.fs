namespace Lisp
module Statements =    
    type Variable = string * System.Type

    type Ast = 
    | Unparsed
    | Number of int
    | Boolean of bool
    | List of Ast list
    | Call of string * Ast list
    | Identifier of string
    | Defun of string * Variable list * Ast

    let rec intVariables = function
    | [] -> []
    | Identifier(name) :: tl -> (name, typeof<int>) :: intVariables tl
    | hd :: tl -> failwith (sprintf "Was expecting list of Identifier, not %A" hd)