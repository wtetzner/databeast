﻿// Copyright 2011, 2012 Walter Tetzner
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

namespace org.bovinegenius.DataBeast
open System.Collections.Generic
open System.Collections
open System.Linq.Expressions
open System.Reflection
open System.Linq
open Printf
open System
open org.bovinegenius.DataBeast.Expression.Match

module PrintExpression =

  let rec print_exps (exps:List<Expression>) =
    String.Join (", ", (exps.Select (fun x -> print_exp x)).ToArray<String>())

  and print_exp (e:Expression) =
    match e with
     | null -> "null"
     | StringConstant (e, o) -> String.Format ("\"{0}\"", o.Replace("\"", "\\\""))
     | Constant (e, typeName, t, o) ->
       match o with
        | null -> "null"
        | _ -> o.ToString()
     | Property (e, o, m, n) -> String.Format("{0}.{1}", print_exp o, n)
     | Index (e, o, arg) -> String.Format("{0}[{1}]", print_exp o, print_exp arg)
     | Parameter (e, name, t) -> name
     | MemberAccess (e, name, o, mem)  -> String.Format ("{0}.{1}", print_exp o, name)
     | FreeVariable (e, t, name) -> name
     | MethodCall (e, name, m, o, args) -> if o = null
                                             then String.Format ("{1}.{0}({2})", name, print_exp (args.ElementAt 0), print_exps (args.Skip(1).ToList()))
                                             else String.Format ("{1}.{0}({2})", name, print_exp o, print_exps (args.ToList()))
     | Equal (e, l, r) -> sprintf "%s == %s" (print_exp l) (print_exp r)
     | AndAlso (e, l, r) -> sprintf "%s && %s" (print_exp l) (print_exp r)
     | OrElse (e, l, r) -> sprintf "%s || %s" (print_exp l) (print_exp r)
     | Binary (e, ExpressionType.Add, l, r) -> sprintf "%s + %s" (print_exp l) (print_exp r)
     | Binary (e, ExpressionType.Subtract, l, r) -> sprintf "%s - %s" (print_exp l) (print_exp r)
     | Binary (e, t, l, r) -> String.Format ("{1} {0} {2}", t.ToString(), print_exp l, print_exp r)
     | Quote (e, o) -> sprintf "QUOTE(%s)" (print_exp o)
     | Unary (e, ExpressionType.Convert, o) -> String.Format ("(({0}){1})", e.Type.Name, print_exp o)
     | Unary (e, t, o) -> String.Format ("{0}({1})", e.Type.ToString(), t.ToString(), print_exp o)
     | Lambda (e, args, body) -> String.Format ("({0}) => {1}", print_exps (args.Cast<Expression>().ToList()), print_exp body)
     | New e -> String.Format ("new {0} {{ {1} }}", e.Constructor.DeclaringType.Name,  String.Join (", ", (Array.map2 (fun arg (mem:MemberInfo) -> String.Format ("{0} = {1}", mem.Name, print_exp arg)) (e.Arguments.ToArray()) (e.Members.ToArray()))))
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

