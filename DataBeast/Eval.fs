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
open org.bovinegenius.DataBeast.Expression.Walk
open org.bovinegenius.DataBeast.PrintExpression
open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions

module Eval =

  let rec evaluate (e:Expression) =
    match e with
     | Constant (c, tname, t, o) -> o
     | _ -> Expression.Lambda(e).Compile().DynamicInvoke(null)

  and constant e = Expression.Constant(e)

  and can_be_evaled (e:Expression) =
    match e with
     | null -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp e, e.NodeType.ToString()))
     | Constant (e, typeName, t, o) -> true
     | RawMethodCall (e, name, m, o, args) -> 
         let obj_evaled = match o with
                           | null -> true
                           | _ -> can_be_evaled o in
         let args_evaled = args.Select(can_be_evaled).All(fun x -> x) in
           obj_evaled && args_evaled
     | Quote (e, o) -> can_be_evaled o
     | Unary (e, t, o) -> can_be_evaled o
     | Binary (e, t, l, r) -> (can_be_evaled l) && (can_be_evaled r)
     | Lambda (e, args, body) -> false
     | Parameter (e, name, t) -> false
     | MemberAccess (e, name, o, mem)  -> can_be_evaled o
     | New e -> e.Arguments.Select(can_be_evaled).All(fun x -> x)
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", print_exp e, e.NodeType.ToString()))

  and partial_eval (e:Expression) =
    walk (fun e ->
           match can_be_evaled e with
            | true -> Some (constant (evaluate e) :> Expression)
            | false -> None) e

