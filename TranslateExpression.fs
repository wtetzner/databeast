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

  let rec translate_to_mysql (e:Expression) =
    match e with
     | NULL e -> ("NULL", [])
     | Quote (e, o) -> translate_to_mysql o
     | DatabaseTable (e, table) -> (table.TableName, [])
     | Constant (c, tname, t, o) -> ("?", [c.Value]) //o.ToString()
     | Equal (e, l, r) -> let (rstr, rvals) = translate_to_mysql r
                            in let (lstr, lvals) = translate_to_mysql l
                                 in match (l, r) with
                                     | (NULL l, r) -> (String.Format ("{0} IS NULL", rstr), rvals)
                                     | (l, NULL r) -> (String.Format ("{0} IS NULL", lstr), lvals)
                                     | _ -> (String.Format ("{0} = {1}", lstr, rstr), List.concat [lvals; rvals])
     | NotEqual (e, l, r) -> let (rstr, rvals) = translate_to_mysql r
                               in let (lstr, lvals) = translate_to_mysql l
                                    in match (l, r) with
                                        | (NULL l, r) -> (String.Format ("{0} IS NOT NULL", rstr), rvals)
                                        | (l, NULL r) -> (String.Format ("{0} IS NOT NULL", lstr), lvals)
                                        | _ -> (String.Format ("{0} <> {1}", lstr, rstr), List.concat [lvals; rvals])
     | LessThan (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                               in (String.Format ("{0} < {1}", lstr, rstr), List.concat [lvals; rvals])
     | GreaterThan (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                                  in (String.Format ("{0} > {1}", lstr, rstr), List.concat [lvals; rvals])
     | LessOrEqual (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                                  in (String.Format ("{0} <= {1}", lstr, rstr), List.concat [lvals; rvals])
     | GreaterOrEqual (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                                     in (String.Format ("{0} >= {1}", lstr, rstr), List.concat [lvals; rvals])
     | Not (e, o) -> let (str, vals) = translate_to_mysql o
                       in (String.Format ("NOT {0}", str), vals)
     | AndAlso (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                              in (String.Format ("{0} AND {1}", lstr, rstr), List.concat [lvals; rvals])
     | OrElse (e, l, r) -> let ((lstr, lvals), (rstr, rvals)) = (translate_to_mysql l, translate_to_mysql r)
                             in (String.Format ("{0} OR {1}", lstr, rstr), List.concat [lvals; rvals])
     | Where (e, o, a) -> let ((ostr, ovals), (astr, avals)) = (translate_to_mysql o, translate_to_mysql a)
                            in (String.Format ("SELECT * FROM {0} WHERE {1}",
                                 (match o with
                                   | DatabaseTable (e, table) -> ostr
                                   | _ -> String.Format ("({0}) AS T", ostr)),
                                 astr), List.concat [ovals; avals])
     | Lambda (e, ps, body) -> translate_to_mysql body
     | Index (e, o, StringConstant (sc, idx)) -> (idx, [])
     | FreeVariable (e, n) -> ("?", [evaluate e])
     | Call (e, "First", o, a) -> let exp = a.Item 0
                                    in let (str, vals) = translate_to_mysql exp
                                         in (String.Format ("SELECT * FROM {0} LIMIT 1",
                                              match exp with
                                               | DatabaseTable (e, table) -> str
                                               | _ -> String.Format ("({0}) AS T", str)), vals)
//     | Call (e, n, o, a) -> if o = null
//                              then translate_to_mysql (a.Item 0)
//                              else translate_to_mysql o
     | _ -> failwith (String.Format ("Unsupported Expression '{0}' of type '{1}'", e.ToString(), e.NodeType.ToString()))

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
