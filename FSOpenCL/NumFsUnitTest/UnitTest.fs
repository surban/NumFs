namespace UnitTestProject1

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open NDArray

[<TestClass>]
type NDArrayTest() = 
    let a1 = NDArray.zeros [2; 3; 20]
    let b1 = NDArray.zeros [3; 3]
    
    do
        b1.|[0;0] <-- 3.14159
        b1.|[1;1] <-- 2.71828
        b1.|[2;2] <-- 1.0
        b1.|[0;2] <-- -1.0

    
    [<TestMethod>]
    member x.Slicing () = 
        let a2 = a1.|[All; Fill; Element(1)] 
        let a3 = a1.|[Element(0); All; Element(1)]
        ()

    [<TestMethod>]
    member x.BroadcastBoth() =
        let a2 = a1.|[All; Fill; Element(1)] 
        NDArray.broadcastBoth a1 a2 |> ignore

    [<TestMethod>]
    member x.Assignment() = 
        a1.|[0;0;1] <-- 342.436234282

    [<TestMethod>]
    member x.ConvertToString() =
        printfn "a1.ToString()=%s" (a1.ToString())

    [<TestMethod>]
    member x.StructuredOutput() = 
        let a2 = a1.|[All; Fill; Element(1)] 
        let a3 = a1.|[Element(0); All; Element(1)]
        printfn "a1 = %A" a1
        printfn "a2 = %A" a2
        printfn "a3 = %A" a3

    [<TestMethod>]
    member x.Arithmetic() =
        printfn "b1 = %A" b1
        printfn "b1 + b1 = %A" (b1 + b1)
        printfn "b1 * b1 = %A" (b1 * b1)
        printfn "b1 ** b1 = %A" (b1 ** b1)


        printfn "b1 + 1. = %A" (b1 + 1.)
        printfn "1. + b1 = %A" (1. + b1)
        
