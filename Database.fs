namespace org.bovinegenius.DataBeast
open System
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

and public DatabaseTableQuery(provider:IQueryProvider, expression:Expression, t:Type) =
  class
    interface IQueryable with
      member x.ElementType = t
      member x.Expression = expression
      member x.Provider = provider  
  end

and public DatabaseTable<'a>(tableName:String, database:Database) as this =
  class
    inherit DatabaseTableQuery(new DatabaseTableQueryProvider<'a>(database), Expression.Constant(this), typedefof<'a>)
      member x.Expression = Expression.Constant(x) :> Expression
      member x.ElementType = typedefof<'a>
      member x.Provider = new DatabaseTableQueryProvider<'a>(database) :> IQueryProvider
    interface IDatabaseTable with
      member x.TableName = tableName
      member x.Database = database
  end

and public DatabaseTableQueryProvider<'a>(database:Database) =
  class
    interface IQueryProvider with
      member x.CreateQuery (e:Expression) = new DatabaseTableQuery(x, e, typedefof<'a>) :> IQueryable
      member x.Execute<'TResult> (e:Expression) = "" :> TResult
  end
