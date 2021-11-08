expression ::=
    | <identifier>
    | <integer>
    | <boolean>
    | <expression> <binary_operator> <expression>
    | (<expression>)
    | let <identifier> = <expression> in <expression>
    | if <expression> then <expression> else <expression>
    | fun <identifier> -> <expression>
    | <expression> <expression>

binary_operator ::=
    | +
    | *

boolean ::=
    | true
    | false
