module Compiler

open System
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

let println expr =
    let rec print expr =
        match expr with
        | Application(expr1, expr2) ->
            // Function application.
            print expr1
            printf " "
            print expr2
        | SpecificCall <@@ (+) @@> (_, _, exprList) ->
            // Matches a call to (+). Must appear before Call pattern.
            print exprList.Head
            printf " + "
            print exprList.Tail.Head
        | Call(exprOpt, methodInfo, exprList) ->
            // Method or module function call. 
            match exprOpt with
            | Some expr -> print expr
            | None -> printf "%s" methodInfo.DeclaringType.Name
            printf ".%s(" methodInfo.Name
            if (exprList.IsEmpty) then printf ")" else
            print exprList.Head
            for expr in exprList.Tail do
                printf ","
                print expr
            printf ")"
        | Int32(n) ->
            printf "%d" n
        | Lambda(param, body) ->
            // Lambda expression.
            printf "fun (%s:%s) -> " param.Name (param.Type.ToString())
            print body
        | Let(var, expr1, expr2) ->
            // Let binding. 
            if (var.IsMutable) then
                printf "let mutable %s = " var.Name
            else
                printf "let %s = " var.Name
            print expr1
            printf " in "
            print expr2
        | PropertyGet(_, propOrValInfo, _) ->
            printf "%s" propOrValInfo.Name
        | String(str) ->
            printf "%s" str
        | Value(value, typ) ->
            printf "%s" (value.ToString())
        | Var(var) ->
            printf "%s" var.Name
        | _ -> printf "%s" (expr.ToString())
    print expr
    printfn "" 


let add x y = x + y
let mul x y = x * y

type FuncInfo = {HostFunc: MethodInfo; TargetFunc: MethodInfo; MetaFunc: MethodInfo}
type TypeInfo = {HostType: Type; MetaType: Type}

let hostFuncs = Dictionary<MethodInfo, FuncInfo>()
let hostTypes = Dictionary<Type, TypeInfo>()

let extractMethodInfo call =
    match call with
    | Call (obj, methodInfo, arguments) -> methodInfo
    | _ -> failwith "quotation must consists of a single call"

let registerTargetFunc hostFunc targetFunc metaFunc =
    hostFuncs.Add(extractMethodInfo hostFunc, 
                  {HostFunc=extractMethodInfo hostFunc
                   TargetFunc=extractMethodInfo targetFunc
                   MetaFunc=extractMethodInfo metaFunc})

let registerType hostType metaType =
    hostTypes.Add(hostType, 
                  {HostType=hostType; MetaType=metaType})

let rec substTargetFuncs expression =
    match expression with
    | Call (oObj, methodInfo, arguments) as e when hostFuncs.ContainsKey(methodInfo) ->
        let fi = hostFuncs.[methodInfo]
        printfn "substituting host function %s with target function %s" methodInfo.Name fi.TargetFunc.Name

        for p in methodInfo.GetParameters() do
            if hostTypes.ContainsKey(p.ParameterType) then
                printfn "found registered type %s" p.ParameterType.Name

        match oObj with
        | Some obj -> Expr.Call (obj, fi.TargetFunc, arguments)
        | None -> Expr.Call (fi.TargetFunc, arguments)       
    | ShapeVar var -> Expr.Var var
    | ShapeLambda (var, expr) -> Expr.Lambda (var, substTargetFuncs expr)
    | ShapeCombination(shapeComboObject, exprList) ->
        RebuildShapeCombination(shapeComboObject, List.map substTargetFuncs exprList)




let compile expr =
    printfn "expr: %A" expr
    println expr


