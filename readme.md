## Regex Implementation

It turns out that using a map to be/hold states results in poor performance. The codebase is (relatively) small and concise, while reaching towards many of the features of PCRE, however, at present it runs 100x slower. I intend to pivot to a different implementation. This one is kept for posterity.

## Grammar

```
expr             :: term (PIPE term)*
term             :: factor+
factor           :: factored_atom | anchor
factored_atom    :: atom quantifier?
atom             :: literal | WILDCARD | group | charset | escape_sequence
quantifier       :: QUESTION | ASTERISK | PLUS | range | nongreedy
nongreedy        :: ASTERISK QUESTION | PLUS QUESTION
range            :: "{" integer ("," integer?)? "}"
group            :: "(" expr ")"
charset          :: "[" CARET? charset_specifier* "]"
anchor           :: CARET | DOLLAR
integer          :: DIGIT+
charset_specifier:: CHAR | range_specifier | escape_sequence
range_specifier  :: integer "-" integer
escape_sequence  :: "\\" (CHAR | metacharacter)
metacharacter    :: "t"|"n"|"r"|"f"|"N"|"b"|"B"|"d"|"D"|"s"|"S"|"w"|"W"|"Q"|"U"|"L"| "c" literal | octal_code
octal_code       :: "0" DIGIT DIGIT
literal          :: DIGIT | ALPHA_LOWER | ALPHA_UPPER | CHAR
CHAR             :: any character except reserved characters or delimiters
DIGIT            :: "0" ..= "9"
ALPHA_LOWER      :: "a" ..= "z"
ALPHA_UPPER      :: "A" ..= "Z"
COMMA            :: ","
DASH             :: "-"
PIPE             :: "|"
PERIOD           :: "."
QUESTION         :: "?"
ASTERISK         :: "*"
PLUS             :: "+"
CARET            :: "^"
DOLLAR           :: "$"
BACKSLASH        :: "\"
```