namespace org.bovinegenius.DataBeast
open org.bovinegenius.DataBeast
open System
open Printf

module Serialize =
    let rec to_mysql query =
      match query with
       | Selection (e, q) -> sprintf "SELECT * FROM (%s) WHERE %s" (to_mysql q) (exp_to_mysql e)
       | Projection (atts, q) -> sprintf "SELECT %s FROM (%s)" (String.Join(", ", List.map attr_to_mysql atts)) (to_mysql q)
       | Relation att -> sprintf "SELECT * FROM %s" (attr_to_mysql att)
       | Limit (q, offset, take) -> sprintf "SELECT * FROM (%s) LIMIT %d,%d" (to_mysql q) offset take

    and exp_to_mysql exp =
        match exp with
            | Or (e1, e2) -> sprintf "(%s) OR (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | And (e1, e2) -> sprintf "(%s) AND (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | Not e -> sprintf "NOT (%s)" (exp_to_mysql e)
            | NotEqual (e1, e2) -> sprintf "(%s) <> (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | Equal (e1, e2) -> sprintf "(%s) = (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | GreaterOrEqual (e1, e2) -> sprintf "(%s) >= (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | LessOrEqual (e1, e2) -> sprintf "(%s) <= (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | LessThan (e1, e2) -> sprintf "(%s) < (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | GreaterThan (e1, e2) -> sprintf "(%s) > (%s)" (exp_to_mysql e1) (exp_to_mysql e2)
            | IsNull e -> sprintf "(%s) IS NULL" (exp_to_mysql e)
            | IsNotNull e -> sprintf "(%s) IS NOT NULL" (exp_to_mysql e)
            | Constant e -> "?"
            | Null -> "NULL"
            | Column a -> attr_to_mysql a

    and attr_to_mysql att =
        match att with
            | Name str -> sprintf "`%s`" (str.Replace("`", "``"))
            | FullName (tbl, col) -> sprintf "`%s`.`%s`" (tbl.Replace("`", "``")) (col.Replace("`", "``"))

    let thing = Projection (([Name "Fred"; Name "Bob"; FullName ("Tab`le", "TheColumn")]),
        (Limit ((Selection ((Or ((Equal ((Column (FullName ("Tab`le", "Column1"))), (Constant 1))), (IsNull (Column (Name "X"))))), (Relation (Name "Table"))),10,20))))
