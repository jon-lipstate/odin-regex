package regex
import "core:strings"
import "core:fmt"

print_nfa :: proc(nfa: ^NFA) {
	for tran, i in nfa.transitions {
		fmt.printf("State: %v, [", i)
		for t in tran {
			fmt.printf("(to = %v, grp = %v, match = %v), ", t.to, t.group, t.match == ɛ ? "Epsilon" : "Mask")
		}
		fmt.printf("]\n")
	}
	fmt.printf("start:%v, end:%v\n", nfa.start, nfa.end)
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
				case (GroupExpr):
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
