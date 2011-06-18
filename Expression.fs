namespace org.bovinegenius.DataBeast.Expression
open System.Linq.Expressions

module Match =
  let (|BinaryExpression|_|) (e:Expression) =
    if e :? BinaryExpression
      then let bin = e :?> BinaryExpression
             in Some (bin.Left, bin.Right)
      else None

  let (|Equal|_|) (e:Expression) =
    if e.NodeType = ExpressionType.Equal
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None
     
  let (|LessThan|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThan
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|GreaterThen|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThan
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
       else None

  let (|LessOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.LessThanOrEqual
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

  let (|GreaterOrEqual|_|) (e:Expression) =
    if e.NodeType = ExpressionType.GreaterThanOrEqual
      then match e with
            | BinaryExpression (l, r) -> Some (l, r)
            | _ -> None
      else None

