module ExprTree

type Function(name: string, body: Block) =
    member this.Name = name
    member this.Body = body

and Var(name: string) =
    member this.Name = name

and Block = Statement list

and Statement =
    | VarSet of Var * Expr
    | Expr of Expr

and Expr =
    | LiteralInt of int
    | LiteralFloat of float
    | LiteralUnit
    | VarGet of Var
    | FuncCall of Function * (Expr list)
    | Ignore of Expr 
