module Env = Map.Make (String)

type op = Add | Mult

type expr =
  | E_Unit
  | E_Int of int
  | E_Bool of bool
  | E_String of string
  | E_Variable of string
  | E_Abstraction of { param : string; body : expr }
  | E_Application of { func : expr; arg : expr }
  | E_Binop of { op : op; left : expr; right : expr }
  | E_Let_in of { param : string; arg : expr; body : expr }
  | E_If_Else of { guard : expr; if_branch : expr; else_branch : expr }

type value =
  | V_Int of int
  | V_Bool of bool
  | V_String of string
  | V_Unit
  | V_Closure of { env : value Env.t; param : string; body : expr }

let string_of_value value =
  match value with
  | V_Int x -> string_of_int x
  | V_Bool x -> string_of_bool x
  | V_String x -> "\"" ^ x ^ "\""
  | V_Unit -> "()"
  | V_Closure _ -> "<function>"
