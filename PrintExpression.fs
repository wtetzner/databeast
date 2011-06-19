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
     | StringConstant (tname, t, o) -> String.Format ("\"{0}\"", o.ToString().Replace("\"", "\\\""))
     | Constant (typeName, t, o) -> String.Format ("(Constant:{0} {1})", typeName, o.ToString())
     | Index (o, arg) -> String.Format("{0}[{1}]", o.ToString(), print_exp arg)
     | FreeVariable (name, o, mem) -> String.Format ("{0}", name)
     | MethodCall (name, m, o, args) -> if o = null
                                          then String.Format ("{1}.{0}({2}))", name, print_exp (args.Item 0), print_exps (args.Skip(1).ToList()))
                                          else String.Format ("{1}.{0}({2})", name, print_exp o, print_exps (args.ToList()))
     | Binary (t, l, r) -> String.Format ("{1} {0} {2}", t.ToString(), print_exp l, print_exp r)
     | UnaryExpression (t, o) -> String.Format ("{0}({1})", t.ToString(), print_exp o)
     | Lambda (args, body) -> String.Format ("(fun ({0}) -> {1})", print_exps (args.Cast<Expression>().ToList()), print_exp body)
     | Parameter (name, t) -> String.Format ("{0}", name, t.ToString())
     | MemberAccess (name, o, mem)  -> String.Format ("{0}.{1}", print_exp o, name)
     | New (cons, args, mem) -> String.Format ("new {0} {{ {1} }}", cons.DeclaringType.Name,  String.Join (", ", (Array.map2 (fun arg (mem:MemberInfo) -> String.Format ("{0} = {1}", mem.Name, print_exp arg)) (args.ToArray()) (mem.ToArray()))))
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))


