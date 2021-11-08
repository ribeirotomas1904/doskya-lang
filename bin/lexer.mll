{
    open Parser

    let remove_quotes string =
        String.sub string 1 (String.length string - 2)
}

let digit = ['0'-'9']
let int = '-'? digit+

let letter = ['a'-'z' 'A'-'Z']
let identifier = letter+

let whitespace = [' ' '\t']+

let string = '"' whitespace? identifier whitespace? '"'

rule read =
    parse
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "true" { TRUE }
    | "false" { FALSE }
    | "()" { UNIT }
    (* also needs to accept empty and all whitespace strings *)
    | string { STRING (Lexing.lexeme lexbuf |> remove_quotes) }
    | "+" { PLUS }
    | "*" { TIMES }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "let" { LET }
    | "in" { IN }
    | "=" { EQUALS }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "fun" { FUN }
    | "->" { ARROW }
    | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
    | whitespace { read lexbuf }
    | eof { EOF }
