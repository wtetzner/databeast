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
open System
open System.Collections
open System.Collections.Generic
open System.Linq
open System.Reflection
open System.Text.RegularExpressions
open org.bovinegenius.DataBeast

module Util =
  let AsDictionary (x:obj)  = match x with
                               | :? IDictionary as d -> d
                               | _ -> let d = new Dictionary<String,Object>()
                                      for p in x.GetType().GetProperties() do
                                        d.Add(p.Name, p.GetValue (x, null))
                                      d :> IDictionary
  let quote_identifier (dbms : Dbms) (ident : String) =
    if (new Regex("^\w+$")).IsMatch(ident)
      then ident
      else match dbms with
            | Dbms.MySql -> sprintf "`%s`" (ident.Replace("`", ""))
            | Dbms.PostgreSql -> sprintf "\"%s\"" (ident.Replace("\"", ""))
            | Dbms.SqlServer -> sprintf "[%s]" (ident.Replace("[", "").Replace("]", ""))
            | _ -> failwith (sprintf "Don't know DBMS '%s'" (dbms.ToString()))

  let InsertStatement (dbms : Dbms, table : String, columns : String[]) =
    let quote = quote_identifier dbms
    let cols = String.Join(",", (columns.Select(quote).ToArray()))
    let vals = String.Join(",", (columns.Select(fun x -> "?").ToArray()))
      in sprintf "INSERT INTO %s (%s) VALUES (%s)" (quote table) cols vals
