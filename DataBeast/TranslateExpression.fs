// Copyright 2011, 2012 Walter Tetzner
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
module M = Expression.Match
open org.bovinegenius.DataBeast.Expression.Walk
open org.bovinegenius.DataBeast.Expression.Eval
open Sql
open PrintExpression
open System.Linq.Expressions
open System.Linq
open System
 
module Translate =
  let rec strip_quotes_in_list (e:IEnumerable<Expression>) = (e.Select strip_quotes).ToArray<Expression>()

  and strip_quotes (e:Expression) =
    match e with
     | null -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp e, e.NodeType.ToString()))
     | M.Quote (e, o) -> strip_quotes e.Operand
     | M.Constant (e, typeName, t, o) -> e :> Expression
     | M.RawMethodCall (e, name, m, o, args) ->
        match o with
         | null -> Expression.Call(o, m, strip_quotes_in_list args) :> Expression
         | _ -> Expression.Call(strip_quotes(o), m, strip_quotes_in_list args) :> Expression
     //| M.Unary (e, ExpressionType.Convert, o) -> strip_quotes o
     | M.Unary (e, t, o) -> Expression.MakeUnary(e.NodeType, strip_quotes(e.Operand), e.Type, e.Method) :> Expression
     | M.Binary (e, t, l, r) -> Expression.MakeBinary(e.NodeType, strip_quotes l, strip_quotes r) :> Expression
     | M.Lambda (e, args, body) -> Expression.Lambda(strip_quotes body, (strip_quotes_in_list (args.Cast<Expression>())).Cast<ParameterExpression>()) :> Expression
     | M.Parameter (e, name, t) -> e :> Expression
     | M.MemberAccess (e, name, o, mem)  -> Expression.MakeMemberAccess(strip_quotes o, mem) :> Expression
     | M.New e -> Expression.New(e.Constructor, strip_quotes_in_list e.Arguments, e.Members) :> Expression
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp e, e.NodeType.ToString()))

      
  let rec to_exp (e:Expression) =
        match e with
        | M.NULL e -> Null
        | M.Quote (e, o) -> to_exp o
        | M.Constant (c, tname, t, o) -> Constant c.Value
        | M.Equal (e, l, r) -> Equal (to_exp l, to_exp r)
        | M.NotEqual (e, l, r) -> NotEqual (to_exp l, to_exp r)
        | M.LessThan (e, l, r) -> LessThan (to_exp l, to_exp r)
        | M.GreaterThan (e, l, r) -> GreaterThan (to_exp l, to_exp r)
        | M.LessOrEqual (e, l, r) -> LessOrEqual (to_exp l, to_exp r)
        | M.GreaterOrEqual (e, l, r) -> GreaterOrEqual (to_exp l, to_exp r)
        | M.Not (e, o) -> Not (to_exp o)
        | M.OrElse (e, l, r) -> Or (to_exp l, to_exp r)
        | M.AndAlso (e, l, r) -> And (to_exp l, to_exp r)
        | M.Lambda (e, ps, body) -> to_exp body
        | M.Index (e, o, M.StringConstant (sc, idx)) ->
            match o with
              | M.Row e ->
                match o with
                 | M.Parameter (e, n, t) -> Column (FullName (n, idx))
                 | _ -> Column (FullName (o.NodeType.ToString(), idx))
              | _ -> Column (Name idx)
         
        | M.FreeVariable (e, t, n) -> Constant (evaluate e)
        | _ -> failwith (sprintf "Unsupported Expression '%s' of type '%s'" (print_exp e) (e.NodeType.ToString()))

  let rec to_query (e:Expression) =
      match e with
       | M.DatabaseTable (e, dt) -> Relation (Name dt.TableName)
       | M.Where (e, o, a) -> Selection (to_exp a, to_query o)
       | M.Call (e, "First", o, a) -> Limit (to_query o, 0, 1)
       | _ -> failwith (String.Format ("Unsupported Expression '{0}' of type '{1}'", print_exp e, e.NodeType.ToString()))

  let merge_wheres exp =
    match exp with
     | M.Call (e, "Where", M.Call (e2, "Where", o, la), a) ->
       //failwith (sprintf "left: %s, right: %s" (la.First().NodeType.ToString()) (a.First().NodeType.ToString()))
       match (strip_quotes (la.First()), strip_quotes (a.First())) with
        | (M.Lambda (le, largs, lbody), M.Lambda (re, rargs, rbody)) ->
          //failwith "HERE!"
          let new_r = walk (fun ex ->
                              match ex with
                               | M.Parameter (pe, pn, pt) -> Some (le.Parameters.First() :> Expression)
                               | _ -> None) rbody in
            Some (Expression.Call(null, e2.Method, [o; (Expression.Quote(Expression.Lambda(Expression.AndAlso(lbody, new_r), largs))) :> Expression]) :> Expression)
        | _ -> None
     | _ -> None

  let remove_quotes exp =
    post_walk (fun x ->
                match x with
                 | M.Quote (e, o) -> Some o
                 | _ -> None) exp

  let collapse_where exp =
    post_walk merge_wheres exp


