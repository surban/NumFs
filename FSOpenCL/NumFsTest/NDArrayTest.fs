open NDArray

let a1 = NDArray.zeros [2; 3; 20]
let a2 = a1.|[All; Fill; Element(1)] 
let a3 = a1.|[Element(0); All; Element(1)]
let (a1b, a2b) = NDArray.broadcastBoth a1 a2

a1.|[0;0;1] <-- 342.436234282

printfn "a1.ToString = %s" (a1.ToString())
printfn "a2.ToString = %s" (a2.ToString())

printfn "a1 = %A" a1
printfn "a2 = %A" a2
printfn "a3 = %A" a3



