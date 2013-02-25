namespace Lisp
module Statements =    
    type Ast = 
    | Unparsed
    | Number of int
    | Boolean of bool
    | Call of string * Ast list
    | Identifier of string
    | Defun of string * Variable list * Ast * Ast

    and Variable = string
