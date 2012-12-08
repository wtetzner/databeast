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
   | Column of Attribute
   | Constant of obj

and AttrList = Attribute list

and Attribute =
   | Name of String
   | FullName of String * String
