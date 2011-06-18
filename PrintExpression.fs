namespace org.bovinegenius.DataBeast
open System.Linq.Expressions
open System
open org.bovinegenius.DataBeast.Expression.Match

module PrintExpression =
  let print_indented (e:Expression) level =
    match e with
     | Constant (tn, o, t) -> String.Format ("{0}(Constant '{1}'", level, tn)
     | _ -> failwith "Don't know expression '{0}'" e.ToString


