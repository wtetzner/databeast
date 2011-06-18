﻿namespace org.bovinegenius.DataBeast.Expression
open System.Linq.Expressions
open System.Reflection;

module Match =
  let (|BinaryExpression|_|) (e:Expression) =
    if e :? BinaryExpression
      then let bin = e :?> BinaryExpression
             in Some (bin.Left, bin.Right)
      else None

  let (|Equal|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Equal
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None
     
  let (|LessThan|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThan
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|GreaterThen|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThan
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
       else None

  let (|LessOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThanOrEqual
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|GreaterOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThanOrEqual
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|NotEqual|_|) (e:Expression) = 
    if e.NodeType = ExpressionType.NotEqual
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|UnaryExpression|_|) (e:Expression) =
    if e :? UnaryExpression
      then let un = e :?> UnaryExpression
             in Some (un.Operand)
      else None

  let (|Not|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Not
      then match e with
            | UnaryExpression (o) -> Some (o)
            | _ -> None
      else None

  let (|MethodCall|_|) (e:Expression) =
    if e :? MethodCallExpression
      then let c = e :?> MethodCallExpression
             in Some (c.Method.Name, c.Method, c.Object, c.Arguments)
      else None
      
  let (|Call|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Call
      then match e with
            | MethodCall (n, m, o, a) -> Some (n, o, a)
            | _ -> None
      else None

  let (|MemberAccess|_|) (e:Expression) =
    if e.NodeType = ExpressionType.MemberAccess
       && e :? MemberExpression
      then let mem = e :?> MemberExpression
             in Some (mem.Member.Name, mem.Expression, mem.Member)
      else None

  let (|PropertyAccess|_|) (e:Expression) =
    match e with
     | MemberAccess (name, obj, mem) -> if mem.MemberType = MemberTypes.Property
                                          then Some (name, obj, mem)
                                          else None
     | _ -> None

  let (|FieldAccess|_|) (e:Expression) =
    match e with
     | MemberAccess (name, obj, mem) -> if mem.MemberType = MemberTypes.Field
                                          then Some (name, obj, mem)
                                          else None
     | _ -> None

  let (|Where|_|) (e:Expression) =
    match e with
     | MethodCall ("Where", m, o, a) -> Some (m, o, a)
     | _ -> None


