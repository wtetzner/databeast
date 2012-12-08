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
open System.Data.Odbc
open System.Data
open org.bovinegenius.DataBeast
open org.bovinegenius.DataBeast.Util
open org.bovinegenius.DataBeast.Expression
open org.bovinegenius.DataBeast.Expression.Match

type public Database(dbms:Dbms, connectionString:String) =
  class
    interface IDatabase with
      member x.ConnectionString = connectionString
      member x.Dbms = dbms
    member x.Item with get (tableName:String) = new DatabaseTable<Row>(dbms, tableName, x)
    member x.BeginTransaction () = let conn = new OdbcConnection(connectionString)
                                      in conn.Open();
                                        conn.BeginTransaction()
    member x.OpenConnection () = let conn = new OdbcConnection(connectionString)
                                    in conn.Open();
                                      conn
    member x.Execute (t : OdbcTransaction, q : String, ([<ParamArray>] args: Object[])) =
      let conn = t.Connection
      let comm = new OdbcCommand(q,conn)
        in comm.Transaction <- t;
            comm.CommandType <- CommandType.Text;
            FillParameters comm args;
            comm.ExecuteNonQuery()
    member x.Execute (q : String, ([<ParamArray>] args: Object[])) =
      let conn = new OdbcConnection(connectionString)
        in conn.Open()
      let t = conn.BeginTransaction()
        in try
              x.Execute(t, q, args)
            finally
              conn.Close()
    member x.Insert (t : OdbcTransaction, table : String, row : obj) =
      let d = AsDictionary(row)
      let comm = new OdbcCommand(InsertStatement(dbms, table, d.Keys.Cast<String>().ToArray()), t.Connection)
        in comm.Transaction <- t;
            comm.CommandType <- CommandType.Text;
            FillParameters comm d.Values;
            comm.ExecuteNonQuery()
    member x.Insert (t : OdbcTransaction, table : String, [<ParamArray>] rows : obj[]) =
      rows.Aggregate(0, fun n row -> n + x.Insert(t, table, row))
    member x.Insert (table : String, [<ParamArray>] rows : obj[]) =
      let conn = new OdbcConnection(connectionString)
      let t = conn.BeginTransaction()
        in try
              let count = rows.Aggregate(0, fun n row -> n + x.Insert(t, table, row))
                in t.Commit();
                  count
            finally
              conn.Close()
    member x.Query (t : OdbcTransaction, q : String, [<ParamArray>] args : obj[]) =
      let comm = new OdbcCommand(q, t.Connection)
        in comm.Transaction <- t;
            comm.CommandType <- CommandType.Text;
            FillParameters comm args
      let items = new OdbcDataAdapter()
      let dataTable = new DataTable()
      let rows = new List<Row>()
        in items.SelectCommand <- comm;
            ignore (items.Fill(dataTable))
      for row in dataTable.Rows do
        rows.Add(new Row(AsDictionary row))
      rows :> IEnumerable<Row>
    member x.Query (q : String, [<ParamArray>] args : obj[]) =
      let conn = new OdbcConnection(connectionString)
        in conn.Open();
      let t = conn.BeginTransaction()
        in try
              let results = x.Query(t, q, args)
                in t.Commit()
              results
            finally
              conn.Close()
  end

and public Row(rowData:IDictionary<String,Object>) =
  class
    interface IDictionary<String,Object> with
      member x.Count = rowData.Count
      member x.Item with get k = rowData.get_Item(k)
                    and  set k v = raise <| NotSupportedException("set_Item is not supported on Row.")
      member x.Keys with get() = rowData.Keys
      member x.Values with get() = rowData.Values
      member x.ContainsKey k = rowData.ContainsKey(k)
      member x.Add (k, v) = raise <| NotSupportedException("Row is immutable; cannot add to it.")
      member x.Remove (k:String) = raise <| NotSupportedException("Row is immutable; cannot remove from it.")
      member x.TryGetValue (k, v) = rowData.TryGetValue(k, ref v)
      member x.IsReadOnly with get() = true
      member x.Add kv = raise <| NotSupportedException("Row is immutable; cannot add to it.")
      member x.Clear() = raise <| NotSupportedException("Row is immutable; cannot clear it.")
      member x.Contains v = rowData.Contains v
      member x.CopyTo (kvs, idx) = rowData.CopyTo (kvs, idx)
      member x.Remove (kv:KeyValuePair<String,Object>) = rowData.Remove kv
      member x.GetEnumerator() = rowData.GetEnumerator()
      member x.GetEnumerator() = rowData.GetEnumerator() :> IEnumerator
    member x.Item with get k = rowData.get_Item(k)
  end

and public DatabaseTableQuery<'a> =
  class
    val mutable provider : IQueryProvider
    val mutable expression : Expression
    new (qprovider:IQueryProvider, texpression:Expression) =
      { provider = qprovider; expression = texpression}

    new (qprovider:IQueryProvider) as this =
      { provider = qprovider; expression = null }
      then
        this.expression <- Expression.Constant(this)
    interface IQueryable<'a> with
      member x.ElementType = typedefof<'a>
      member x.Expression = x.expression
      member x.Provider = x.provider
      member x.GetEnumerator() = (x.provider.Execute(x.expression) :?> IEnumerable<'a>).GetEnumerator()
      member x.GetEnumerator() = (x.provider.Execute(x.expression) :?> IEnumerable).GetEnumerator()
    interface IOrderedQueryable<'a>
  end

and public DatabaseTable<'a>(dbms:Dbms, tableName:String, database:Database) =
  class
    inherit DatabaseTableQuery<'a>(new DatabaseTableQueryProvider<'a>(dbms, database))
      member x.Expression = Expression.Constant(x) :> Expression
      member x.ElementType = typedefof<'a>
      member x.Provider = new DatabaseTableQueryProvider<'a>(dbms, database) :> IQueryProvider
    interface IDatabaseTable with
      member x.TableName = tableName
      member x.Database = database :> IDatabase
    member x.Insert (row : Object) = database.Insert(tableName, row)
    member x.Insert (t : OdbcTransaction, row : Object) = database.Insert(t, tableName, row)
  end

and public DatabaseTableQueryProvider<'a>(dbms:Dbms , database:Database) as this =
  class
    interface IQueryProvider with
      member x.Execute<'TResult> (e:Expression) = this.Execute(e) :?> 'TResult
      member x.Execute (e:Expression) = this.Execute(e)
      member x.CreateQuery<'TElement> (e:Expression) = new DatabaseTableQuery<'TElement>(x, e) :> IQueryable<'TElement>
      member x.CreateQuery (e:Expression) = new DatabaseTableQuery<'a>(x, e) :> IQueryable
    member x.Execute (e:Expression) = let (sql, args) = Translate.translate_to_sql dbms e
                                      let results = database.Query(sql, args.ToArray())
                                        in match e with
                                            | Call (e, "First", o, a) -> results.First() :> obj
                                            | _ -> results :> obj
  end
