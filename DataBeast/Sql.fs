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
open System.Linq;

module Sql =

    type Query =
    | Intersection of Query * Query
    | Union of Query * Query
    | Difference of Query * Query
    | NaturalJoin of Query * Query
    | LeftJoin of Query * Query
    | RightJoin of Query * Query
    | CartesianProduct of Query * Query
    | Selection of Exp * Query
    | Projection of AttrList * Query
    | Relation of Attribute
    | Limit of Query * int * int
    | Query of Attribute * Query

    and Exp = 
    | Or of Exp * Exp
    | And of Exp * Exp
    | Not of Exp
    | NotEqual of Exp * Exp
    | Equal of Exp * Exp
    | GreaterOrEqual of Exp * Exp
    | LessOrEqual of Exp * Exp
    | LessThan of Exp * Exp
    | GreaterThan of Exp * Exp
    | IsNull of Exp
    | IsNotNull of Exp
    | Null
    | Column of Attribute
    | Constant of obj

    and AttrList = Attribute list

    and Attribute =
    | Name of String
    | FullName of String * String

    let rec query_to_consts query =
      match query with
       | Projection (atts, q) -> query_to_consts q
       | Selection (e, q) -> List.append (exp_to_constants e) (query_to_consts q)
       | Relation att -> []
       | Limit (q, s, e) -> query_to_consts q

    and exp_to_constants exp =
      match exp with
       | Constant c -> [c]
       | Or (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | And (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | NotEqual (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | Equal (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | GreaterOrEqual (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | LessOrEqual (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | LessThan (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | GreaterThan (e1, e2) -> List.append (exp_to_constants e1) (exp_to_constants e2)
       | Not e -> exp_to_constants e
       | IsNull e -> exp_to_constants e
       | IsNotNull e -> exp_to_constants e
       | Null -> []
       | Column att -> []

    and query_to_constants query =
        let consts = query_to_consts query in
          consts.AsEnumerable()