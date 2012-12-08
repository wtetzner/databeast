// Copyright 2011 Walter Tetzner
//
// This file is part of DataBeast.
//
// DataBeast is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// DataBeast is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with DataBeast.  If not, see <http://www.gnu.org/licenses/>.

namespace org.bovinegenius.DataBeast.Expression
open org.bovinegenius.DataBeast
open System.Linq.Expressions
open System.Linq
open System.Reflection;
open System

module Match =
  let (|Binary|_|) (e:Expression) =
    if e :? BinaryExpression
      then let bin = e :?> BinaryExpression
             in Some (bin, bin.NodeType, bin.Left, bin.Right)
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

  let (|GreaterThan|_|) (e:Expression) =
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
             in Some (un, un.NodeType, un.Operand)
      else None

  let (|Not|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Not
      then match e with
            | Unary (e, t, o) -> Some (e, o)
            | _ -> None
      else None

  let (|MethodCall|_|) (e:Expression) =
    if e :? MethodCallExpression
      then let c = e :?> MethodCallExpression
             in Some (c, c.Method.Name, c.Method, c.Object, c.Arguments)
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
             in Some (mem, mem.Member.Name, mem.Expression, mem.Member)
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
     | MethodCall (e, "Where", m, o, a) -> Some (e, a.First(), a.Skip(1).First())
     | _ -> None

  let (|Select|_|) (e:Expression) =
    match e with
     | MethodCall (e, "Select", m, o, a) -> Some (e, m, o, a.Item 1)
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
             in Some (c, c.Type.Name, c.Type, c.Value)
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
             in Some (par, par.Name, par.Type)
      else None

  let (|Index|_|) (e:Expression) =
    match e with
     | MethodCall (e, "get_Item", m, o, a) -> Some (e, o, a.Item 0)
     | _ -> None

  let (|Lambda|_|) (e:Expression) =
    if e :? LambdaExpression
      then let l = e :?> LambdaExpression
             in Some (l, l.Parameters, l.Body)
      else None

  let (|FreeVariable|_|) (e:Expression) =
    if e :? MemberExpression
      then let m = e :?> MemberExpression
             in if m.Expression.NodeType <> ExpressionType.Parameter
                  then Some (m, m.Member.Name)
                  else None
      else None

  let (|StringConstant|_|) (e:Expression) =
    match e with
     | Constant (e, tname, t, o) -> if o :? String
                                      then Some (e, o :?> String)
                                      else None
     | _ -> None

  let (|NumberConstant|_|) (e:Expression) =
    match e with
     | Constant (e, tname, t, o) -> if o :? int || o :? float || o :? double || o :? byte
                                      then Some (e, o)
                                      else None
     | _ -> None

  let (|IQueryable|_|) (e:Expression) =
    match e with
     | Constant (e, tname, t, o) -> if o :? IQueryable
                                      then Some (e, o :?> IQueryable)
                                      else None
     | _ -> None

  let (|DatabaseTable|_|) (e:Expression) =
    match e with
     | IQueryable (e, o) -> if o :? IDatabaseTable
                              then Some (e, o :?> IDatabaseTable)
                              else None
     | _ -> None

  let (|New|_|) (e:Expression) =
    if e :? NewExpression
      then let n = e :?> NewExpression
             in Some n
      else None

  let (|Quote|_|) (e:Expression) =
    match e with
     | Unary (e, t, o) -> if t = ExpressionType.Quote
                            then Some (e, o)
                            else None
     | _ -> None

  let (|NULL|_|) (e:Expression) =
    match e with
     | Constant (e, tname, t, o) -> if o = null
                                      then Some e
                                      else None
     | _ -> None
