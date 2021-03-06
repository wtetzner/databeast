﻿// Copyright 2011 Walter Tetzner
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
open System.Linq
open System.Collections.Generic

type Dbms = 
   | SqlServer = 1u
   | MySql = 2u
   | PostgreSql = 3u

type IDatabase =
  interface
    abstract ConnectionString : String
    abstract Dbms : Dbms
  end

and IDatabaseTable =
  interface
    inherit IOrderedQueryable
    inherit IQueryable
    abstract TableName : String
    abstract Database : IDatabase
  end
