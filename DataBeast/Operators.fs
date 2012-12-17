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

namespace org.bovinegenius.DataBeast
open System
open System.Collections
open System.Collections.Generic
open System.Linq;
open System.Linq.Expressions;
open org.bovinegenius.DataBeast.Expression.Match
open org.bovinegenius.DataBeast.PrintExpression
open System.Collections
open System.Reflection;

module Operators =

    type table_name = string
    type column_name = string

    type reference = Table of table_name
                   | Column of table_name * column_name

    type path = PathParam of string
              | PathProperty of path * string
              | PathIndex of path * string

    let rec expression_to_path (e:Expression) =
      match e with
       | Parameter (e, name, t) -> Some (PathParam name)
       | Property (e, o, prop, name) ->
         let result = expression_to_path o in
           match result with
            | None -> None
            | Some exp -> Some (PathProperty (exp, name))
       | Index (e, o, a) ->
         let name = (a :?> ConstantExpression).Value :?> String in
         let result = expression_to_path o in
           match result with
            | None -> None
            | Some exp -> Some (PathIndex (exp, name))
       | _ -> None

    type environment = { param : string; paths : Dictionary<path, reference> }

    let inline dict s =
      let coll = Dictionary<path, reference>() in
        Seq.iter (fun (k,v) -> coll.Add(k, v)) s;
        coll

    let rec environment exp =
        match exp with
         | DatabaseTable (e, dt) -> { param = "obj"; paths = dict [PathParam "obj", Table dt.TableName] }
         | Where (e, o, a) -> environment o
         | Call (e, "OrderBy", o, a) -> environment o
         | Call (e, "OrderByDescending", o, a) -> environment o
         | Call (e, "ThenBy", o, a) -> environment o
         | Call (e, "ThenByDescending", o, a) -> environment o
         | Call (e, "SelectMany", o, a) -> 
        
    and update_env env param =
      match env with
       | PathParam name -> param
       | PathProperty (path, name) -> PathProperty ((update_env path param), name)
       | PathIndex (path, name) -> PathIndex ((update_env path param), name)


