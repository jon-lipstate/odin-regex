package regex
///
import "core:fmt"
import "core:unicode/utf8"

Parser :: struct {
	runes:  []rune,
	offset: int,
	expr:   Expr,
}
Parse_Error :: enum {
	Ok,
	Not_Of_Type, //      Didnt Match
	Unexpected_Token, // Bad Syntax
	Unexpected_EOF, //   File Ends before expected (Bad Syntax)
}

init_parser :: proc(expr_string: string, allocator := context.allocator) -> Parser {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	p := Parser {
		runes  = utf8.string_to_runes(expr_string, allocator),
		offset = 0,
	}
	return p
}
peek :: proc(p: ^Parser, lookahead := 0) -> (ch: rune, eof: bool = false) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	if lookahead != 0 {panic("not implemented")}
	if p.offset >= len(p.runes) {
		eof = true
	} else {
		ch = p.runes[p.offset]
	}
	return
}
// returns current `ch`, advance offset
advance :: proc(p: ^Parser) -> (ch: rune, eof: bool = false) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	if p.offset >= len(p.runes) {
		eof = true
	} else {
		ch = p.runes[p.offset]
		p.offset += 1
	}
	return
}
parse_expr :: proc(p: ^Parser, allocator := context.allocator) -> (expr: Expr, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("parse_expr")
	expr = Expr{make([dynamic]Term)}
	for {
		term: Term
		term, err = parse_term(p, allocator)
		if err != .Ok {
			// todo: check other stuff and zero len?
			break
		}
		append(&expr.terms, term)
		ch, eof := peek(p)
		if !eof && ch == '|' {
			advance(p)
		} else {break}
	}
	if len(expr.terms) == 0 {
		delete(expr.terms)
		err = .Not_Of_Type
	}
	return
}
parse_term :: proc(p: ^Parser, allocator := context.allocator) -> (term: Term, err: Parse_Error = .Ok) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("parse_term")
	term = Term{make([dynamic]Factor)}
	for {
		factor, f_err := parse_factor(p, allocator)
		if f_err != .Ok {
			err = f_err // bubble up??
			break
		}
		append(&term.factors, factor)
		ch, eof := peek(p)
		if eof || !(is_atom_start(ch) || is_anchor(ch) || !is_metacharacter(ch)) {
			break
		}
	}
	if len(term.factors) == 0 {
		delete(term.factors)
		err = .Not_Of_Type
	}
	return
}
parse_factor :: proc(p: ^Parser, allocator := context.allocator) -> (f: Factor, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_factor", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	if ch == '^' {
		f = Anchor_Start{}
		advance(p)
	} else if ch == '$' {
		f = Anchor_End{}
		advance(p)
	} else {
		// fmt.println("parse_factor - FactoredAtom")
		fa: FactoredAtom
		fa.atom, err = parse_atom(p, allocator)
		ch, eof = peek(p)
		// fmt.println("parse_atom - err", fa.atom, err, rune(ch), eof)
		if err == .Ok && (ch == '?' || ch == '*' || ch == '+' || ch == '{') {
			// fmt.println("parse_factor - parse_quantifier")
			fa.quantifier, err = parse_quantifier(p)
		}
		f = fa
	}
	return
}
parse_quantifier :: proc(p: ^Parser) -> (q: Quantifier, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_quantifier", rune(ch))
	if eof {err = .Not_Of_Type;return}
	switch ch {
	case '?':
		q.range = {
			min = 0,
			max = 1,
		}
		advance(p)
	case '*':
		q.range = {
			min = 0,
			max = -1,
		}
		advance(p)
	case '+':
		q.range = {
			min = 1,
			max = -1,
		}
		advance(p)
	case '{':
		range, err := parse_range(p)
		if err != .Ok {return}
		q.min = range.min
		q.max = range.max
	case:
		err = .Not_Of_Type
		return
	}
	ch, eof = peek(p)
	if !eof && ch == '?' {
		q.nongreedy = true
		advance(p)
	}
	return
}
parse_range :: proc(p: ^Parser) -> (r: Range, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_range", rune(ch))
	if eof || ch != '{' {err = .Not_Of_Type;return}
	advance(p) // eat '{'
	r.min, err = parse_integer(p)
	if err == .Not_Of_Type {err = .Unexpected_Token;return}
	ch, eof = peek(p)
	if eof {err = .Unexpected_Token;return}
	if ch == ',' {
		advance(p) // eat ','
		ch, eof = peek(p)
		if eof {err = .Unexpected_Token;return}
		if ch == '}' {
			r.max = -1
		} else {
			r.max, err = parse_integer(p)
			if err == .Not_Of_Type {err = .Unexpected_Token;return}
			ch, eof = advance(p)
			if eof || ch != '}' {err = .Unexpected_Token;return}
		}
	} else if ch == '}' {
		r.max = r.min // must be single value eg {3} -> Range{3,3}
		advance(p) // eat '}'
	}
	return
}
parse_integer :: proc(p: ^Parser) -> (n: int, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_integer", rune(ch))
	if eof || !is_digit(ch) {err = .Not_Of_Type;return}
	for {
		digit := int(ch - '0')
		n = n * 10 + digit
		advance(p)
		ch, eof := peek(p)
		if eof {break}
		if !is_digit(ch) {break}
	}
	return
}
parse_atom :: proc(p: ^Parser, allocator := context.allocator) -> (atom: Atom, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_atom", rune(ch))
	if eof {err = .Not_Of_Type;return}
	switch ch {
	case '\\':
		esc: EscapeSequence
		esc, err = parse_escape_sequence(p)
		if err == .Ok {
			// simplify any 'easy' types:
			#partial switch e in esc {
			case (rune):
				atom = e
			case (Codepoint):
				atom = rune(e)
			case (OctalCode):
				atom = rune(e)
			case:
				atom = esc
			}
		}
	case '.':
		advance(p)
		atom = Wildcard{}
	case '(':
		atom, err = parse_group(p, allocator)
		if err != .Ok {err = .Unexpected_Token}
	case '[':
		advance(p)
		atom, err = parse_charset(p, allocator)
		if err != .Ok {err = .Unexpected_Token}
	case:
		atom, err = parse_literal(p)
	}
	return
}
// 0-9, \d
is_digit :: proc(ch: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	return ch >= '0' && ch <= '9'
}
// ( [ . \
is_atom_start :: proc(ch: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	return ch == '(' || ch == '[' || ch == '.' || ch == '\\'
}
// ^$
is_anchor :: proc(ch: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	return ch == '^' || ch == '$'
}
// // \w
// is_word :: proc(ch: u8) -> bool {
// 	return (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
// }
parse_charset :: proc(p: ^Parser, allocator := context.allocator) -> (charset: Charset, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_charset", rune(ch))
	if eof {err = .Not_Of_Type;return}
	if ch == '^' {
		charset.caret = true
		advance(p) // eat '^'
	}
	charset.specifiers = make([dynamic]CharsetSpecifier, allocator)
	for {
		ch, eof = peek(p)
		if eof {err = .Unexpected_Token;break}
		if ch == ']' {advance(p);break}
		specifier, s_err := parse_charset_specifier(p)
		if s_err != .Ok {err = .Unexpected_Token;break}
		append(&charset.specifiers, specifier)
	}
	if err != .Ok && len(charset.specifiers) == 0 {delete(charset.specifiers)}
	return
}
parse_charset_specifier :: proc(p: ^Parser) -> (spec: CharsetSpecifier, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_charset_specifier", rune(ch))
	if eof {err = .Not_Of_Type;return}
	if ch == '\\' {
		escape_sequence, p_err := parse_escape_sequence(p)
		if p_err != .Ok {err = p_err;return}
		spec = escape_sequence
	} else {
		literal, l_err := parse_literal(p)
		if l_err != .Ok {err = l_err;return}
		ch, eof := peek(p)
		if !eof && ch == '-' {
			advance(p) // eat '-'
			literal_end, le_err := parse_literal(p)
			if le_err != .Ok {err = le_err;return}
			spec = Range{int(literal), int(literal_end)}
		} else {
			spec = literal
		}
	}
	if r, rok := spec.(Literal); rok {spec = r} 	// capture escaped runes
	return
}
parse_escape_sequence :: proc(p: ^Parser) -> (esc: EscapeSequence, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	advance(p) // Consume the backslash
	ch, eof := advance(p)
	if eof {err = .Unexpected_EOF;return}

	switch ch {
	case 'P', 'p':
		esc = PropertyEscape.Invalid
		panic("property escape not imple")
	case 'd', 'D', 's', 'S', 'w', 'W', 'b', 'B', 'n', 'N', 't', 'r', 'f', 'Q', 'E':
		// \U requires special handling for unicode escape, not impl
		cmd := parse_regex_command(ch)
		if cmd == .Invalid {err = .Unexpected_Token;return}
		esc = cmd
	case '0' ..= '7':
		octal_code, o_err := parse_octal_code(p)
		if o_err != .Ok {err = o_err;return}
		esc = Codepoint(octal_code) // TODO: Verify no reason to keep as octal distinct type
	case 'c':
		control_char, c_err := parse_control_char(p)
		if c_err != .Ok {err = c_err;return}
		esc = ControlChar(control_char)
	case 'x', 'u', 'U':
		unicode_code, u_err := parse_unicode_code(p)
		if u_err != .Ok {err = u_err;return}
		esc = unicode_code
	case '\\', '|', '(', ')', '{', '}', '[', ']', '^', '$', '.', '*', '+', '?':
		esc = rune(ch)
	case:
		err = .Unexpected_Token
	}
	return
}
parse_unicode_code :: proc(p: ^Parser) -> (c: Codepoint, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := advance(p) // eat prefix
	// fmt.println("parse_unicode_code", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	n_digits: int
	// TODO: switch to \x{FFAB}
	// https://www.regular-expressions.info/unicode.html#codepoint
	switch ch {
	case 'x':
		n_digits = 2
	case 'u':
		n_digits = 4
	case 'U':
		n_digits = 8
	case:
		err = .Unexpected_Token;return
	}
	c = 0
	for i := 0; i < n_digits; i += 1 {
		ch, eof = advance(p)
		if eof {err = .Unexpected_EOF;return}
		digit := 0
		if '0' <= ch && ch <= '9' {
			digit = int(ch - '0')
		} else if 'a' <= ch && ch <= 'f' {
			digit = 10 + int(ch - 'a')
		} else if 'A' <= ch && ch <= 'F' {
			digit = 10 + int(ch - 'A')
		} else {
			err = .Unexpected_Token;return
		}
		c = (c << 4) + Codepoint(digit)
	}
	return
}
parse_control_char :: proc(p: ^Parser) -> (cc: ControlChar, err: Parse_Error) {
	// TRACE(&spall_ctx, &spall_buffer, #procedure)
	advance(p) // eat the 'c'
	ch, eof := advance(p)
	// fmt.println("parse_control_char", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	// Only accept '@', 'A'-'Z', or 'a'-'z' for control characters
	if ch == '@' || ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z') {
		// https://www.regular-expressions.info/nonprint.html
		// cc = ControlChar(ch & 0x1F) // i dont think this is right
	} else {
		err = .Unexpected_Token // Error: Invalid control character
	}
	// return
	unimplemented()

}
parse_octal_code :: proc(p: ^Parser) -> (n: int, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := advance(p) // eat '0'
	// fmt.println("parse_octal_code", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	// Parse the octal value
	n = 0
	for i := 0; i < 3; i += 1 {
		ch, eof := peek(p)
		if eof || (ch < '0' || ch > '7') {
			break // unsure when err and when done
		}
		n = n * 8 + int(ch - '0')
		advance(p)
	}
	if n > 255 {
		err = .Unexpected_Token // diff err?
		return
	}
	return
}
parse_literal :: proc(p: ^Parser) -> (literal: Literal, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_literal", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	if ch == '\\' {
		ch, eof = peek(p)
		if eof {err = .Unexpected_EOF;return}
		if is_metacharacter(ch) {
			advance(p)
			literal = rune(ch)
			// fmt.println(literal, rune(ch))
		} else {
			start_offset := p.offset
			esc, esc_err := parse_escape_sequence(p)
			if esc_err != .Ok {err = esc_err;return}
			// convert to simple literal:
			if s, sok := esc.(rune); sok {literal = s} else {
				// p.offset = start_offset // backup, this was a bad parse
				err = .Not_Of_Type
				// TODO: im not sure what to do with this branch tbh
			}
		}
	} else if is_metacharacter(ch) {
		err = .Not_Of_Type
		return
	} else {
		advance(p)
		literal = rune(ch)
	}
	return
}
is_metacharacter :: proc(ch: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	switch ch {
	case '\\', '|', '(', ')', '[', ']', '{', '}', '?', '*', '+', '^', '$', '.':
		return true
	case:
		return false
	}
}
parse_regex_command :: proc(ch: rune) -> RegexCommand {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	switch ch {
	case 'd':
		return .Any_Digit
	case 'D':
		return .Not_Digit
	case 's':
		return .Any_Whitespace
	case 'S':
		return .Not_Whitespace
	case 'w':
		return .Any_Word
	case 'W':
		return .Not_Word
	// case 'b':
	// 	return .Assert_Boundary
	// case 'B':
	// 	return .Not_Boundary
	case 'n':
		return .Newline
	case 'N':
		return .Not_Newline
	case 't':
		return .Tab
	case 'r':
		return .Carriage_Return
	// case 'f':
	// 	return .Formfeed
	// case 'Q':
	// 	return .EscQ
	// case 'E':
	// 	return .EscE
	// case 'U':
	// 	return .UppercaseLetter
	// case 'L':
	// 	return .LowercaseLetter
	case:
		return .Invalid
	}
}
parse_group :: proc(p: ^Parser, allocator := context.allocator) -> (group: GroupExpr, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_group", rune(ch))
	if eof {err = .Unexpected_EOF;return}
	if ch != '(' {err = .Unexpected_Token;return} 	// not of type??
	advance(p) // eat '('
	expr: Expr
	expr, err = parse_expr(p, allocator)
	group = GroupExpr(expr)
	if err != .Ok {return}
	ch, eof = peek(p)
	if eof {err = .Unexpected_EOF;return}
	if ch != ')' {err = .Unexpected_Token;return}
	advance(p) // eat ')'
	return
}
parse_anchor :: proc(p: ^Parser) -> (f: Factor, err: Parse_Error) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	ch, eof := peek(p)
	// fmt.println("parse_anchor", rune(ch))
	if eof {err = .Not_Of_Type;return}
	if ch == '^' {
		f = Anchor_Start{}
		advance(p) // eat '^'
	} else if ch == '$' {
		f = Anchor_End{}
		advance(p) // eat '$'
	} else {
		err = .Not_Of_Type
	}
	return
}
destroy_expr :: proc(expr: ^Expr) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	for term in &expr.terms {
		for factor in &term.factors {
			if fa, ok := &factor.(FactoredAtom); ok {
				#partial switch fa in &fa.atom {
				case (GroupExpr):
					grp := Expr(fa)
					destroy_expr(&grp)
				case (Charset):
					delete(fa.specifiers)
				}
			}
		}
		delete(term.factors)
	}
	delete(expr.terms)
	expr.terms = nil
}
Expr :: struct {
	terms: [dynamic]Term,
}
Term :: struct {
	factors: [dynamic]Factor,
}
Factor :: union {
	Anchor_Start,
	Anchor_End,
	FactoredAtom,
}
FactoredAtom :: struct {
	atom:       Atom,
	quantifier: Maybe(Quantifier),
}
Atom :: union {
	Literal,
	GroupExpr,
	Charset,
	Wildcard,
	EscapeSequence,
}
Wildcard :: struct {}
Anchor_Start :: struct {}
ANCHOR_START :: Anchor_Start{}
Anchor_End :: struct {}
ANCHOR_END :: Anchor_End{}
Quantifier :: struct {
	using range: Range,
	nongreedy:   bool,
}
Range :: struct {
	min: int,
	max: int,
}
GroupExpr :: distinct Expr
Charset :: struct {
	specifiers: [dynamic]CharsetSpecifier,
	caret:      bool,
}
Anchor :: struct {
	caret:  bool,
	dollar: bool,
}
Integer :: string
Literal :: rune
CharsetSpecifier :: union {
	Literal, // always removed at AST-parse time
	Range,
	EscapeSequence,
}
EscapeSequence :: union {
	RegexCommand, // eg `\s`
	rune, // eg `\\`
	ControlChar, // eg `\cM`
	OctalCode, // eg `\072`
	Codepoint, // eg `\u{1F600}`
	PropertyEscape, // eg `\p{L}`
}
ControlChar :: distinct int
OctalCode :: distinct int
Codepoint :: distinct int
RegexCommand :: enum {
	Invalid,
	Any_Digit = 'd',
	Not_Digit = 'D',
	Any_Whitespace = 's',
	Not_Whitespace = 'S',
	Any_Word = 'w',
	Not_Word = 'W',
	// Assert_Boundary = 'b',
	// Not_Boundary = 'B',
	Newline = 'n',
	Not_Newline = 'N',
	Tab = 't',
	Carriage_Return = 'r',
	// Formfeed = 'f',
	// EscQ = 'Q',
	// EscE = 'E',
	// UppercaseLetter = 'U',
	// LowercaseLetter = 'L',
}
PropertyEscape :: enum {
	Invalid, // NOT IMPLEMENTED
	// Letter, // \p{L} - Any letter (in any script).
	// Mark, // \p{M} - Any mark (e.g., diacritics, accents).
	// Number, // \p{N} - Any number (e.g., digits, fractions).
	// Punctuation, // \p{P} - Any punctuation character.
	// Symbol, // \p{S} - Any symbol (e.g., currency, math).
	// Separator, // \p{Z} - Any separator (e.g., space, line, paragraph).
	// Other, // \p{C} - Any other character (e.g., control, private use).
	// DecimalDigit, // \p{Nd} - Any decimal digit (in any script).
	// Greek, // \p{Greek} - Any character in the Greek script.
	// Latin, // \p{Latin} - Any character in the Latin script.
	// Cyrillic, // \p{Cyrillic} - Any character in the Cyrillic script.
	// Lowercase, // \p{Lowercase} - Any lowercase character.
	// Uppercase, // \p{Uppercase} - Any uppercase character.
	// Titlecase, // \p{Titlecase} - Any titlecase character.
	// Cased, // \p{Cased} - Any character that has a case (lowercase, uppercase, or titlecase).
	// Whitespace, // \p{Whitespace} - Any whitespace character (e.g., space, tab, newline).
}
