namespace Lisp
module Statements =    
    type Ast = 
    | Unparsed
    | Number of int
    | Boolean of bool
    | List of Ast list
    | Call of string * Ast list
    | Identifier of string
    | Defun of string * Ast list * Ast