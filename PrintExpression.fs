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

namespace org.bovinegenius.DataBeast
open System.Collections.Generic
open System.Collections
open System.Linq.Expressions
open System.Reflection
open System.Linq
open System
open org.bovinegenius.DataBeast.Expression.Match

module PrintExpression =

  let rec print_exps (exps:List<Expression>) =
    String.Join (", ", (exps.Select (fun x -> print_exp x)).ToArray())

  and print_exp (e:Expression) =
    match e with
     | StringConstant (e, o) -> String.Format ("\"{0}\"", o.ToString().Replace("\"", "\\\""))
     | Constant (e, typeName, t, o) -> String.Format ("(Constant:{0} {1})", typeName, o.ToString())
     | Index (e, o, arg) -> String.Format("{0}[{1}]", print_exp o, print_exp arg)
     | FreeVariable (e, name, o, mem) -> String.Format ("{0}", name)
     | MethodCall (e, name, m, o, args) -> if o = null
                                             then String.Format ("{1}.{0}({2})", name, print_exp (args.Item 0), print_exps (args.Skip(1).ToList()))
                                             else String.Format ("{1}.{0}({2})", name, print_exp o, print_exps (args.ToList()))
     | Binary (e, t, l, r) -> String.Format ("{1} {0} {2}", t.ToString(), print_exp l, print_exp r)
     | Quote (e, t, o) -> print_exp o
     | Unary (e, t, o) -> String.Format ("{0}({1})", t.ToString(), print_exp o)
     | Lambda (e, args, body) -> String.Format ("({0}) => {1}", print_exps (args.Cast<Expression>().ToList()), print_exp body)
     | Parameter (e, name, t) -> String.Format ("{0}", name)
     | MemberAccess (e, name, o, mem)  -> String.Format ("{0}.{1}", print_exp o, name)
     | New (e, cons, args, mem) -> String.Format ("new {0} {{ {1} }}", cons.DeclaringType.Name,  String.Join (", ", (Array.map2 (fun arg (mem:MemberInfo) -> String.Format ("{0} = {1}", mem.Name, print_exp arg)) (args.ToArray()) (mem.ToArray()))))
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

