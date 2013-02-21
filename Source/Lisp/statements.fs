namespace Lisp
module Statements =    
    type Ast = 
    | Number of int
    | Boolean of bool
    | Call of string * Ast list
    