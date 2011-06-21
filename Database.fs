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
open System.Collections.Generic
open System.Collections
open System.Linq
open System.Linq.Expressions

type Dbms = 
   | SqlServer = 1u
   | MySql = 2u
   | PostgreSql = 3u

type public Database(dbms:Dbms, connectionString:String) =
  class
    
  end

and IDatabaseTable =
  interface
    inherit IQueryable
    abstract TableName : String
    abstract Database : Database
  end

and public DatabaseTableQuery<'a>(provider:IQueryProvider, expression:Expression) =
  class
    interface IQueryable<'a> with
      member x.ElementType = typedefof<'a>
      member x.Expression = expression
      member x.Provider = provider
      member x.GetEnumerator() = (provider.Execute(expression) :?> IEnumerable<'a>).GetEnumerator()
      member x.GetEnumerator() = (provider.Execute(expression) :?> IEnumerable).GetEnumerator()
  end

and public DatabaseTable<'a>(tableName:String, database:Database) as this =
  class
    inherit DatabaseTableQuery<'a>(new DatabaseTableQueryProvider<'a>(database), Expression.Constant(this))
      member x.Expression = Expression.Constant(x) :> Expression
      member x.ElementType = typedefof<'a>
      member x.Provider = new DatabaseTableQueryProvider<'a>(database) :> IQueryProvider
    interface IDatabaseTable with
      member x.TableName = tableName
      member x.Database = database
  end

and public DatabaseTableQueryProvider<'a>(database:Database) as this =
  class
    interface IQueryProvider with
      member x.Execute<'TResult> (e:Expression) = this.Execute(e) :?> 'TResult
      member x.Execute (e:Expression) = this.Execute(e)
      member x.CreateQuery<'TElement> (e:Expression) = new DatabaseTableQuery<'TElement>(x, e) :> IQueryable<'TElement>
      member x.CreateQuery (e:Expression) = new DatabaseTableQuery<'a>(x, e) :> IQueryable
    member x.Execute (e:Expression) = "" :> obj
  end
