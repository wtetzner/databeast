namespace org.bovinegenius.DataBeast
open System
open System.Linq

type Dbms = 
   | SqlServer = 1u
   | MySql = 2u
   | PostgreSql = 3u

type DatabaseTable =
  interface
    inherit IQueryable
    abstract TableName : String
  end