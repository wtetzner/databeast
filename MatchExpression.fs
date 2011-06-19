namespace org.bovinegenius.DataBeast.Expression
open System.Linq.Expressions
open System.Reflection;
open System

module Match =
  let (|Binary|_|) (e:Expression) =
    if e :? BinaryExpression
      then let bin = e :?> BinaryExpression
             in Some (e, bin.NodeType, bin.Left, bin.Right)
      else None

  let (|Equal|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Equal
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None
     
  let (|LessThan|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThan
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|GreaterThen|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThan
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
       else None

  let (|LessOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThanOrEqual
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|GreaterOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThanOrEqual
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|NotEqual|_|) (e:Expression) = 
    if e.NodeType = ExpressionType.NotEqual
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|Unary|_|) (e:Expression) =
    if e :? UnaryExpression
      then let un = e :?> UnaryExpression
             in Some (e, un.NodeType, un.Operand)
      else None

  let (|Not|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Not
      then match e with
            | Unary (e, t, o) -> Some (e, t, o)
            | _ -> None
      else None

  let (|MethodCall|_|) (e:Expression) =
    if e :? MethodCallExpression
      then let c = e :?> MethodCallExpression
             in Some (e, c.Method.Name, c.Method, c.Object, c.Arguments)
      else None
      
  let (|Call|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Call
      then match e with
            | MethodCall (e, n, m, o, a) -> Some (e, n, o, a)
            | _ -> None
      else None

  let (|MemberAccess|_|) (e:Expression) =
    if e.NodeType = ExpressionType.MemberAccess
       && e :? MemberExpression
      then let mem = e :?> MemberExpression
             in Some (e, mem.Member.Name, mem.Expression, mem.Member)
      else None
      
  let (|PropertyAccess|_|) (e:Expression) =
    match e with
     | MemberAccess (e, name, obj, mem) ->
         if mem.MemberType = MemberTypes.Property
           then Some (e, name, obj, mem)
           else None
     | _ -> None

  let (|FieldAccess|_|) (e:Expression) =
    match e with
     | MemberAccess (e, name, obj, mem) ->
         if mem.MemberType = MemberTypes.Field
           then Some (e, name, obj, mem)
           else None
     | _ -> None

  let (|Where|_|) (e:Expression) =
    match e with
     | MethodCall (e, "Where", m, o, a) -> Some (e, m, o, a.Item 0)
     | _ -> None

  let (|Select|_|) (e:Expression) =
    match e with
     | MethodCall (e, "Select", m, o, a) -> Some (e, m, o, a.Item 0)
     | _ -> None

  let (|SelectMany|_|) (e:Expression) =
    match e with
     | MethodCall (e, "SelectMany", m, o, a) -> Some (e, m, o, a)
     | _ -> None

  let (|Aggregate|_|) (e:Expression) =
    match e with
     | MethodCall (e, "Aggregate", m, o, a) -> Some (e, m, o, a)
     | _ -> None

  let (|Constant|_|) (e:Expression) =
    if e :? ConstantExpression
      then let c = e :?> ConstantExpression
             in Some (e, c.Type.Name, c.Type, c.Value)
      else None

  let (|AndAlso|_|) (e:Expression) =
    if e.NodeType = ExpressionType.AndAlso
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|OrElse|_|) (e:Expression) =
    if e.NodeType = ExpressionType.OrElse
      then match e with
            | Binary (e, t, l, r) -> Some (e, l, r)
            | _ -> None
      else None

  let (|Parameter|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Parameter
      then let par = e :?> ParameterExpression
             in Some (e, par.Name, par.Type)
      else None

  let (|Index|_|) (e:Expression) =
    match e with
     | MethodCall (e, "get_Item", m, o, a) -> Some (e, o, a.Item 0)
     | _ -> None

  let (|Lambda|_|) (e:Expression) =
    if e :? LambdaExpression
      then let l = e :?> LambdaExpression
             in Some (e, l.Parameters, l.Body)
      else None

  let (|FreeVariable|_|) (e:Expression) =
    if e :? MemberExpression
      then let m = e :?> MemberExpression
             in if m.NodeType <> ExpressionType.Parameter
                  then Some (e, m.Member.Name, m.Expression, m.Member)
                  else None
      else None

  let (|StringConstant|_|) (e:Expression) =
    match e with
     | Constant (e, tname, t, o) -> if o :? String
                                      then Some (e, tname, t, o)
                                      else None
     | _ -> None

  let (|New|_|) (e:Expression) =
    if e :? NewExpression
      then let n = e :?> NewExpression
             in Some (e, n.Constructor, n.Arguments, n.Members)
      else None

  let (|Quote|_|) (e:Expression) =
    match e with
     | Unary (e, t, o) -> if t = ExpressionType.Quote
                            then Some (e, t, o)
                            else None
     | _ -> None
