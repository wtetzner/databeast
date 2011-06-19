namespace org.bovinegenius.DataBeast.Expression
open System.Collections.Generic
open org.bovinegenius.DataBeast
open org.bovinegenius.DataBeast.Expression.Match
open System.Linq.Expressions
open System.Linq
open System
 
module Translate =
  type sql_translation = Sql of String * Object list

  let rec strip_quotes_in_list (e:IEnumerable<Expression>) =
    (e.Select (fun x -> strip_quotes x)).ToArray()

  and strip_quotes (e:Expression) =
    match e with
     | Constant (e, typeName, t, o) -> e :> Expression
     | MethodCall (e, name, m, o, args) -> Expression.Call(strip_quotes(o), m, strip_quotes_in_list args) :> Expression
     | Quote (e, t, o) -> e.Operand
     | Unary (e, t, o) -> Expression.MakeUnary(e.NodeType, strip_quotes(e.Operand), e.Type, e.Method) :> Expression
     | Lambda (e, args, body) -> Expression.Lambda(strip_quotes body, (strip_quotes_in_list (args.Cast<Expression>())).Cast<ParameterExpression>().ToArray()) :> Expression
     | Parameter (e, name, t) -> e :> Expression
     | MemberAccess (e, name, o, mem)  -> Expression.MakeMemberAccess(strip_quotes o, mem) :> Expression
     | New (e, cons, args, mem) -> Expression.New(cons, strip_quotes_in_list args, mem) :> Expression
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

  let translate_to_mysql (e:Expression) =
    match e with
     | StringConstant (e, str) -> String.Format ("'{0}'", str.Replace("'", "''"))
     | NumberConstant (e, num) -> num.ToString()
     | DatabaseTable (e, table) -> table.TableName
     | _ -> failwith (String.Format ("Don't know expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

  let translate_to_sqlserver (e:Expression) =
    failwith "Dbms 'SqlServer' is currently unsupported."

  let translate_to_postgresql (e:Expression) =
    failwith "Dbms 'PostgreSQL' is currently unsupported."

  let translate_to_sql (dbms:Dbms) (e:Expression) =
    match dbms with
     | Dbms.MySql -> translate_to_mysql e
     | Dbms.SqlServer -> translate_to_sqlserver e
     | Dbms.PostgreSql -> translate_to_postgresql e
     | _ -> failwith (String.Format ("DBMS '{0}' no supported", dbms.ToString()))
     
  

