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
open System.Reflection;

module Operators =

    type table_name = string
    type column_name = string

    type reference = Table of table_name
                   | Column of table_name * column_name

    type environment = { current_obj : Expression; paths : Dictionary<Expression, reference> }

    let rec environment exp =
        match exp with
         | DatabaseTable e ->  e
        
    //and update_env env exp =

//    let environment env exp =
//        match exp with
//         | 
//
//    let exp_fold func env exp value =
//        match exp with
//         | Call (e, n, o, a) ->
//            let new_value = func env exp value
