open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

type typ =
  | TInt
  | TBool

module type TypeContext = sig
  type t

  val empty : t

  val lookup : t -> string -> typ

  val extend : t -> string -> typ -> t
end

module TypeContext : TypeContext = struct
  type t = (string * typ) list

  let empty = []

  let lookup ctx x = (try List.assoc x ctx with Not_found -> failwith "unbound value")

  let extend ctx x ty = (x, ty) :: ctx
end

let rec typeof ctx = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> TypeContext.lookup ctx x
  | Let (x, e1, e2) -> typeof_let ctx x e1 e2
  | Binop (bop, e1, e2) -> typeof_bop ctx bop e1 e2
  | If (e1, e2, e3) -> typeof_if ctx e1 e2 e3

and typeof_let ctx x e1 e2 =
  let t1 = typeof ctx e1 in
  let ctx' = TypeContext.extend ctx x t1 in
  typeof ctx' e2

and typeof_bop ctx bop e1 e2 =
  let (t1, t2) = (typeof ctx e1, typeof ctx e2) in
  match (bop, t1, t2) with
  | (Add, TInt, TInt) -> TInt
  | (Mult, TInt, TInt) -> TInt
  | (Leq, TInt, TInt) -> TBool
  | _ -> failwith "Operator and operand type mismatch"

and typeof_if ctx e1 e2 e3 =
  if typeof ctx e1 = TBool then
    let (t2, t3) = (typeof ctx e2, typeof ctx e3) in
    if t2 = t3 then
      t3
    else
      failwith "If branches must have the same type"
  else
    failwith "Guard must be a boolean"

let typecheck e = ignore (typeof TypeContext.empty e)

let is_value : expr -> bool = function
  | Int _
  | Bool _ ->
    true
  | Var _
  | Binop _
  | Let _
  | If _ ->
    false

let rec subst e v x =
  match e with
  | Var y ->
    if x = y then
      v
    else
      e
  | Bool _
  | Int _ ->
    e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    if y = x then
      Let (y, subst e1 v x, e2)
    else
      Let (y, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

let rec step : expr -> expr = function
  | Int _
  | Bool _ ->
    failwith "Doesn't step"
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith "Guard type mismatch, it should be bool"
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_bop bop e1 e2 =
  match (bop, e1, e2) with
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | (Leq, Int a, Int b) -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"

let rec eval_small e =
  if is_value e then
    e
  else
    e |> step |> eval_small

let rec eval_big e =
  match e with
  | Int _
  | Bool _ ->
    e
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | If (e1, e2, e3) -> eval_if e1 e2 e3

and eval_bop bop e1 e2 =
  match (bop, eval_big e1, eval_big e2) with
  | (Add, Int a, Int b) -> Int (a + b)
  | (Mult, Int a, Int b) -> Int (a * b)
  | (Leq, Int a, Int b) -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"

and eval_if e1 e2 e3 =
  match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | _ -> failwith "if gurad should be bool"
