package regex
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:prof/spall"
spall_ctx: spall.Context
spall_buffer: spall.Buffer

ENABLE_SPALL :: #config(ENABLE_SPALL, false)

TRACE :: #force_inline proc (ctx: ^spall.Context, buf: ^spall.Buffer, name: string) {
	when ENABLE_SPALL {
		spall.SCOPED_EVENT(ctx, buf, name)
	}
}

///
main :: proc() {
	// spall_ctx = spall.context_create("regex.spall")
	// defer spall.context_destroy(&spall_ctx)
	// buffer_backing := make([]u8, spall.BUFFER_DEFAULT_SIZE)
	// spall_buffer = spall.buffer_create(buffer_backing)
	// defer spall.buffer_destroy(&spall_ctx, &spall_buffer)
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	regex := "([0-9]+)-([0-9]+)-([0-9]+)"
	// regex := "a|b"
	p := init_parser(regex)
	// AST of Regex Inputs:
	expr, err := parse_expr(&p);defer destroy_expr(&expr)
	nfa := compile_nfa(expr);defer destroy_nfa(&nfa)
	for k, v in nfa.transitions {fmt.println(k, v)}
	fmt.printf("start:%v, end:%v\n", nfa.start, nfa.end)

	str := "650-253-0001"
	m := match(&nfa, str)
	// fmt.printf("regex:\"%s\", str:\"%s\", matches: %v\n", regex, str, m)

	// sb := strings.builder_make()
	// print_ast(&expr, &sb)
	// fmt.println(strings.to_string(sb))
}

tests := map[string]string {
	"\\w+"                        = "testing_123",
	"0x[0-9a-fA-F_]{2,}"          = "0x2A19_42DD",
	"(a|b)+c?d*"                  = "abababcdddd", // "cd" false
	"[a-zA-Z_]\\w*\\s*::\\s*proc" = "foo :: proc",
}

// Recursively fills a `sb` with the ast
print_ast :: proc(expr: ^Expr, sb: ^strings.Builder, depth := 0) {
	depth := depth
	if depth > 0 {
		strings.write_string(sb, strings.repeat(" ", depth))
	}
	strings.write_string(sb, "Expr(")
	strings.write_int(sb, len(expr.terms))
	strings.write_string(sb, ")\n")
	depth += 2
	for term in expr.terms {
		depth -= 1
		strings.write_string(sb, strings.repeat(" ", depth))
		strings.write_string(sb, "Term(")
		strings.write_int(sb, len(term.factors))
		strings.write_string(sb, ")\n")
		depth += 1
		for factor in term.factors {
			strings.write_string(sb, strings.repeat(" ", depth))
			strings.write_string(sb, "Factor: ")
			switch f in factor {
			case (Anchor_Start):
				strings.write_string(sb, "(Anchor_Start)")
			case (Anchor_End):
				strings.write_string(sb, "(Anchor_End)")
			case (FactoredAtom):
				q, qok := f.quantifier.?
				switch a in f.atom {
				case (Group):
					strings.write_string(sb, "Group(\n")
					grp := Expr(a)
					print_ast(&grp, sb, depth + 1)
					strings.write_string(sb, strings.repeat(" ", depth))
					strings.write_string(sb, ")")
				case (rune):
					strings.write_string(sb, "Literal '")
					strings.write_rune(sb, a)
					strings.write_string(sb, "'")
				case (Charset):
					strings.write_string(sb, "[")
					if a.caret {
						strings.write_string(sb, "^")
					}
					for cs in a.specifiers {
						switch s in cs {
						case (EscapeSequence):
							if rc, rok := s.(RegexCommand); rok {
								strings.write_rune(sb, rune('\\'))
								strings.write_rune(sb, rune(rc))
							} else {
								esc := fmt.tprintf("\\%v", s)
								strings.write_string(sb, esc)
							}
						case (Range):
							strings.write_rune(sb, rune(s.min))
							if s.max != s.min {
								strings.write_rune(sb, rune('-'))
								strings.write_rune(sb, rune(s.max))
							}
						case (rune):
							strings.write_rune(sb, s)
						}

					}

					strings.write_string(sb, "]")

				case (Wildcard):
					strings.write_string(sb, ".")
				case (EscapeSequence):
					if rc, rok := a.(RegexCommand); rok {
						strings.write_rune(sb, rune('\\'))
						strings.write_rune(sb, rune(rc))
					} else {
						esc := fmt.tprintf("\\%v", a)
						strings.write_string(sb, esc)
					}
				}
				if qok {
					if q.min == 0 && q.max == 1 {
						strings.write_string(sb, "?")
					} else if q.min == 0 && q.max == -1 {
						strings.write_string(sb, "*")
					} else if q.min == 1 && q.max == -1 {
						strings.write_string(sb, "+")
					} else {
						qrange := fmt.tprintf("{%v,%v}", q.min, q.max)
						strings.write_string(sb, qrange)
					}
					if q.nongreedy {
						strings.write_string(sb, "?")
					}
				}

			}
			strings.write_string(sb, "\n")
		}
	}
}
