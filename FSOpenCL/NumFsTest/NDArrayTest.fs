open Microsoft.FSharp.Quotations

open NDArray
open Compiler

let a1 = NDArray.zeros [3; 3]
a1.|[0;0] <-- 3.14159
a1.|[1;1] <-- 2.71828
a1.|[2;2] <-- 1.0
a1.|[0;2] <-- -1.0


type ViewMeta = {a: float}
let a1m = {a=0.}

[<ReflectedDefinition>]
let testHost (a: View) = a + 1.

[<ReflectedDefinition>]
let testTarget (a: View) = 2. * a

[<ReflectedDefinition>]
let testMeta (a: ViewMeta) = {a=2.*a.a}

registerTargetFunc <@ testHost a1 @> <@ testTarget a1 @> <@ testMeta a1m @>
registerType typeof<View> typeof<ViewMeta>


let b = 3
let input = <@ 3. + testHost a1  @>
printfn "original expr:     %A" input
let output = substTargetFuncs input 
printfn "substituted expr:  %A" output


