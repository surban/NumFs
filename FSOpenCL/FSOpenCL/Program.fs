open FSCL
open FSCL.Compiler
open FSCL.Language

[<ReflectedDefinition>]
let VectorAdd(a: float32[], b:float32[], c:float32[], wi: WorkItemInfo) =
    let myId = wi.GlobalID(0)
    c.[myId] <- a.[myId] + b.[myId]

// Instantiate the compiler
let compiler = new Compiler()
// Pass a kernel
let resultCompilingRef = compiler.Compile(<@ VectorAdd @>)
// Or a kernel call
let a = Array.create 1024 2.0f
let b = Array.create 1024 3.0f
let c = Array.zeroCreate<float32> 1024
let size = WorkSize(a.LongLength, 64L)
let resultCompilingCall = compiler.Compile(<@ VectorAdd(a, b, c, size) @>)

resultCompilingCall

