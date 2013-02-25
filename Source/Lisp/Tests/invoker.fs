module tests.intepreter

open Xunit
open FsUnit.Xunit

open tests.parser

// help method
let invoke<'a> = parse >> (Invoker.invoke<'a> Invoker.framework)
