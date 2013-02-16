// include Fake libs
#I @"packages\FAKE.1.74.8.0\tools"
#r "FakeLib.dll"
open Fake 

// tools
let fslex = @"packages\FSPowerPack.Community.3.0.0.0\Tools\fslex.exe"
let fsyacc = @"packages\FSPowerPack.Community.3.0.0.0\Tools\fsyacc.exe"
let xunit = @"packages\xunit.runners.1.9.1\tools\xunit.console.clr4.exe"

// properties
let buildDir = @".\build\"
let lexerDir = @".\Source\Lisp\"
let parserDir = @".\Source\Lisp\"

// projects
let projs = !+ @"**\*.csproj" ++ @"**\*.fsproj" |> Scan

// Clean
Target "Clean" (fun _ ->
    CleanDir buildDir
)

// FsParse
Target "FsYacc" (fun _ ->
    let result =
        ExecProcess (fun info -> 
            info.FileName <- fsyacc
            info.Arguments <- parserDir + "parser.fsy --module Parser"
        ) (System.TimeSpan.FromMinutes 1.)

    if result <> 0 then failwith "Failed result from fsyacc"
)

// FsLex
Target "FsLex" (fun _ ->
    let result =
        ExecProcess (fun info ->
            info.FileName <- fslex
            info.Arguments <- lexerDir + "lexer.fsl --unicode"
        ) (System.TimeSpan.FromMinutes 1.)

    if result <> 0 then failwith "Failed result from fslex"
)

// xUnit
Target "xUnit" (fun _ ->
    let result =
        ExecProcess (fun info ->
            info.FileName <- xunit
            info.Arguments <- buildDir + "Lisp.dll"
        ) (System.TimeSpan.FromMinutes 1.)

    if result <> 0 then failwith "Failed result from xUnit"
)

// Default target
Target "Build" (fun _ ->
    MSBuildRelease buildDir "Build" projs |> Log "AppBuild-Output: "
)

// Dependencies
"Clean"
  ==> "FsLex"
  ==> "FsYacc"
  ==> "Build"
  ==> "xUnit"

// start build
Run "xUnit"