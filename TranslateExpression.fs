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
open System.Collections.Generic
open org.bovinegenius.DataBeast
open org.bovinegenius.DataBeast.Expression.Match
open System.Linq.Expressions
open System.Linq
open System
 
module Translate =
  let rec strip_quotes_in_list (e:IEnumerable<Expression>) =
    (e.Select (fun x -> strip_quotes x)).ToArray()

  and strip_quotes (e:Expression) =
    match e with
     | Constant (e, typeName, t, o) -> e :> Expression
     | MethodCall (e, name, m, o, args) -> Expression.Call(strip_quotes(o), m, strip_quotes_in_list args) :> Expression
     | Quote (e, o) -> e.Operand
     | Unary (e, t, o) -> Expression.MakeUnary(e.NodeType, strip_quotes(e.Operand), e.Type, e.Method) :> Expression
     | Lambda (e, args, body) -> Expression.Lambda(strip_quotes body, (strip_quotes_in_list (args.Cast<Expression>())).Cast<ParameterExpression>().ToArray()) :> Expression
     | Parameter (e, name, t) -> e :> Expression
     | MemberAccess (e, name, o, mem)  -> Expression.MakeMemberAccess(strip_quotes o, mem) :> Expression
     | New e -> Expression.New(e.Constructor, strip_quotes_in_list e.Arguments, e.Members) :> Expression
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

  let rec evaluate (e:Expression) =
    match e with
      | Constant (c, tname, t, o) -> o
      | _ -> Expression.Lambda(e).Compile().DynamicInvoke(null)

      
  let rec to_exp (e:Expression) =
        match e with
        | NULL e -> Null
        | Quote (e, o) -> to_exp o
        | Constant (c, tname, t, o) -> Constant c.Value
        | Equal (e, l, r) -> Equal (to_exp l, to_exp r)
        | NotEqual (e, l, r) -> NotEqual (to_exp l, to_exp r)
        | LessThan (e, l, r) -> LessThan (to_exp l, to_exp r)
        | GreaterThan (e, l, r) -> GreaterThan (to_exp l, to_exp r)
        | LessOrEqual (e, l, r) -> LessOrEqual (to_exp l, to_exp r)
        | GreaterOrEqual (e, l, r) -> GreaterOrEqual (to_exp l, to_exp r)
        | Not (e, o) -> Not (to_exp o)
        | OrElse (e, l, r) -> Or (to_exp l, to_exp r)
        | AndAlso (e, l, r) -> And (to_exp l, to_exp r)
        | Lambda (e, ps, body) -> to_exp body
        | Index (e, o, StringConstant (sc, idx)) -> match sc with
                                                     | DatabaseTable (e, table) -> Column (FullName (table.TableName, idx))
                                                     | _ -> Column (Name idx)
        | FreeVariable (e, n) -> Constant (evaluate e)

  let rec to_query (e:Expression) =
    match e with
     | DatabaseTable (e, table) -> Relation (Name table.TableName)
     | Where (e, o, a) -> Selection (to_exp a, to_query o)
     | Call (e, "First", o, a) -> Limit (to_query o, 0, 1)
//     | Call (e, n, o, a) -> if o = null
//                              then translate_to_mysql (a.Item 0)
//                              else translate_to_mysql o
     | _ -> failwith (String.Format ("Unsupported Expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))



