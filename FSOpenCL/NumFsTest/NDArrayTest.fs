open Microsoft.FSharp.Quotations

open NDArray
open Translator

let a1 = NDArray.zeros [3; 3]
a1.|[0;0] <-- 3.14159
a1.|[1;1] <-- 2.71828
a1.|[2;2] <-- 1.0
a1.|[0;2] <-- -1.0

[<ReflectedDefinition>]
let add x y = 
    1 |> ignore
    x + y

[<ReflectedDefinition>]
let mul x y = x * y


translate <@ add 1 2 @>

