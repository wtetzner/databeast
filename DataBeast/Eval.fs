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

namespace org.bovinegenius.DataBeast.Expression
open org.bovinegenius.DataBeast.Expression.Match
open org.bovinegenius.DataBeast.Expression.Walk
open System
open System.Collections.Generic
open System.Linq
open System.Linq.Expressions

module Eval =

  let rec evaluate (e:Expression) =
    match e with
     | Constant (c, tname, t, o) -> o
     | _ -> Expression.Lambda(e).Compile().DynamicInvoke(null)

  and eval_tables (e:Expression) =
    walk (fun e ->
           match e with
            | DatabaseTable e -> Some (Expression.Constant(evaluate e) :> Expression)
            | _ -> None) e

