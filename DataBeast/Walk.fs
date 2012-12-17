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
open org.bovinegenius.DataBeast.Expression.Match
open org.bovinegenius.DataBeast.PrintExpression
open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions

module Walk =

    let rec walk_list func (exps:IEnumerable<Expression>) = exps.Select(fun x -> walk func x).ToArray<Expression>()

    and walk func exp =
      let result = func exp in
        match result with
         | None ->
           match exp with
            | null -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp exp, exp.NodeType.ToString()))
            | Constant (e, typeName, t, o) -> exp
            | RawMethodCall (e, name, m, o, args) -> 
              match o with
               | null ->
                 let new_args = walk_list func args in
                   Expression.Call(o, m, new_args) :> Expression
               | _ -> Expression.Call(walk func o, m, walk_list func args) :> Expression
            | Quote (e, o) -> Expression.Quote(walk func e.Operand) :> Expression
            | Unary (e, t, o) -> Expression.MakeUnary(e.NodeType, walk func e.Operand, e.Type, e.Method) :> Expression
            | Binary (e, t, l, r) -> Expression.MakeBinary(e.NodeType, walk func l, walk func r) :> Expression
            | Lambda (e, args, body) -> Expression.Lambda(e.Type, walk func body, (walk_list func (args.Cast<Expression>())).Cast<ParameterExpression>()) :> Expression
            | Parameter (e, name, t) -> e :> Expression
            | MemberAccess (e, name, o, mem)  -> Expression.MakeMemberAccess(walk func o, mem) :> Expression
            | New e -> Expression.New(e.Constructor, walk_list func e.Arguments, e.Members) :> Expression
            | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp exp, exp.NodeType.ToString()))
         | Some v -> v

    and post_walk func exp =
      walk (fun e ->
             let result = func e in
               match result with
                | None -> None
                | Some v ->
                  if v = e then
                    None
                  else
                    Some (post_walk func v)) exp

    and replace obj1 obj2 exp =
      walk (fun e ->
             if obj1 = e then
               Some obj2
             else
               None) exp