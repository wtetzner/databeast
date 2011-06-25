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

type Exp =
   | Intersection of Exp * Exp
   | Union of Exp * Exp
   | SetDifference of Exp * Exp
   | NaturalJoin of Exp * Exp
   | LeftJoin of Exp * Exp
   | RightJoin of Exp * Exp
   | CartesianProduct of Exp * Exp
   | Selection of CondList * Exp
   | Projection of AttrList * Exp
   | Relation of Rel
   | Constant of obj

and CondList = 
   | Or of CondList * CondList
   | And of CondList * CondList
   | Not of CondList
   | NotEqual of Exp * Exp
   | Equal of Exp * Exp
   | GreaterOrEqual of Exp * Exp
   | LessOrEqual of Exp * Exp
   | LessThan of Exp * Exp
   | GreaterThan of Exp * Exp

and AttrList = Attribute list

and Rel = String

and Attribute =
   | Name of String
   | FullName of String * String