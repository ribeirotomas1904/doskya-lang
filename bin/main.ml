open Ast

let parse input = input |> Lexing.from_string |> Parser.program Lexer.read

let initial_env = Env.empty

(* TODO: use cps tranformation *)
let rec eval env expr =
  match expr with
  | E_Int x -> V_Int x
  | E_Bool x -> V_Bool x
  | E_String x -> V_String x
  | E_Unit -> V_Unit
  | E_Variable x -> (
      match Env.find_opt x env with Some x' -> x' | None -> failwith "TODO")
  | E_Abstraction { param; body } -> V_Closure { env; param; body }
  | E_Application { func; arg } -> (
      print_endline "evaluating application";
      let value = eval env arg in
      let func' = eval env func in
      match func' with
      | V_Closure { env; param; body } ->
          let env' = Env.add param value env in
          eval env' body
      | _ -> failwith "TODO")
  | E_Binop { op; left; right } -> (
      let left_v = eval env left in
      let right_v = eval env right in
      match op with
      | Add -> (
          match (left_v, right_v) with
          | V_Int a, V_Int b -> V_Int (a + b)
          | _ -> failwith "TODO")
      | Mult -> (
          match (left_v, right_v) with
          | V_Int a, V_Int b -> V_Int (a * b)
          | _ -> failwith "TODO"))
  | E_Let_in { param; arg; body } ->
      let value = eval env arg in
      let env' = Env.add param value env in
      eval env' body
  | E_If_Else { guard; if_branch; else_branch } -> (
      let bool = eval env guard in
      match bool with
      | V_Bool x -> if x then eval env if_branch else eval env else_branch
      | _ -> failwith "TODO")

let () =
  "let f = fun x -> x x in f f" |> parse |> eval initial_env |> string_of_value
  |> print_endline
