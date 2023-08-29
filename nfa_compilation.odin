package regex
//
import "core:fmt"

// TODO(jon): See if i can remove sets entirely?
import "./set"
Set :: set.Set
//
Pair :: struct(T: typeid) {
	a, b: T,
}
Automata :: struct {
	start: int,
	end:   int,
}
NFA :: struct {
	// Invariant: All `Transitions` are sorted such that ɛ-transitions are at the beginning.
	transitions:   [dynamic]Transitions,
	start:         int,
	end:           int,
	// regexy-features:
	next_group_id: int, // used to produce group ids; zero is reserved for not-a-group
	global:        bool,
	//unimplemented features:
	fold_case:     bool,
}
Transitions :: [dynamic]Transition
Transition :: struct {
	to:    int,
	group: int, // 0: not a group, -i:start, i:end
	match: MatchKind,
}
Epsilon :: struct {}
ɛ :: Epsilon{}
MatchKind :: union {
	Epsilon,
	rune,
	Rune_Class,
	Rune_Set,
	Anchor_Start,
	Anchor_End,
}

Rune_Set :: struct {
	runes:  []Rune_Set_Item,
	invert: bool,
}
Rune_Set_Item :: union {
	rune,
	Rune_Range,
	Rune_Class,
}
Rune_Range :: struct {
	start, end: rune,
}
Rune_Class :: enum {
	Any,
	Not_Newline,
	// Alpha,
	Digit,
	Not_Digit,
	// XDigit,
	// Numeric,
	Word,
	Not_Word,
	Space,
	Not_Space,

	// Punctuation,
}

make_nfa :: proc(allocator := context.allocator) -> NFA {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	context.allocator = allocator
	nfa := NFA {
		transitions   = make([dynamic]Transitions),
		start         = -1,
		end           = -1,
		next_group_id = 1,
	}
	return nfa
}
destroy_nfa :: proc(nfa: ^NFA) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	for v in nfa.transitions {
		delete(v)
	}
	delete(nfa.transitions)
}
add_state :: proc(nfa: ^NFA, allocator := context.allocator) -> int {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	context.allocator = allocator
	id := len(nfa.transitions)
	append(&nfa.transitions, Transitions{})
	return id
}
compile_nfa :: proc(ast: Expr, allocator := context.allocator) -> NFA {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	context.allocator = allocator
	nfa := make_nfa()
	nfa.start = add_state(&nfa)
	nfa.end = compile_expr(&nfa, ast, nfa.start)
	return nfa
}
compile_expr :: proc(nfa: ^NFA, expr: Expr, start: int, allocator := context.allocator) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("compile_expr", start)
	context.allocator = allocator
	assert(len(expr.terms) > 0)
	if len(expr.terms) == 1 {
		end = compile_term(nfa, expr.terms[0], start)
	} else {
		terms := make([]int, len(expr.terms))
		defer delete(terms)
		for term, i in expr.terms {
			// ts := add_state(nfa)
			te := compile_term(nfa, term, start)
			terms[i] = te
		}
		// end = compile_alt_array(nfa, terms, start)
		end = add_state(nfa)
		for te in terms {
			add_transition(nfa, te, end, ɛ)
		}
	}
	return
}
compile_term :: proc(nfa: ^NFA, term: Term, start: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("compile_term", start)
	assert(len(term.factors) > 0)
	current_end := start
	// Factors are Concatenated
	for factor in term.factors {
		current_end = compile_factor(nfa, factor, current_end)
	}
	end = current_end
	return
}
compile_factor :: proc(nfa: ^NFA, factor: Factor, start: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("compile_factor", start)
	switch f in factor {
	case (Anchor_Start):
		end = add_state(nfa)
		add_transition(nfa, start, end, ANCHOR_START)
		panic("Anchor_Start - NOT WORKING")
	case (Anchor_End):
		end = add_state(nfa)
		add_transition(nfa, start, end, ANCHOR_END)
		panic("Anchor_End - NOT WORKING")
	case (FactoredAtom):
		quant := f.quantifier
		switch atom in f.atom {
		case (Literal):
			end = compile_rune(nfa, atom, start)
		case (GroupExpr):
			end = compile_expr(nfa, Expr(atom), start)
			// fmt.println("GroupExpr", start, end)
			// for t in &nfa.transitions[start] {
			// 	t.group = -1 * nfa.next_group_id
			// 	// fmt.println("start-t", t)
			// }
			// for t in &nfa.transitions[end] {
			// 	t.group = 1 * nfa.next_group_id
			// 	// fmt.println("end-t", t)
			// }
			// We want transitions going into our end state to close the group:
			// NOTE: This didnt work out - phone test is repro
			// for trans, s in &nfa.transitions {
			// 	for t in &trans {
			// 		if t.to == end && t.to != start {
			// 			t.group = 1 * nfa.next_group_id
			// 			fmt.println("end-t", t)
			// 		}
			// 	}
			// }
			nfa.next_group_id += 1
		case (Charset):
			end = compile_charset(nfa, atom, start)
		case (Wildcard):
			end = compile_wildcard(nfa, start)
		case (EscapeSequence):
			end = compile_charset(nfa, Charset{{atom}, false}, start)
		}

		if quant != nil {
			q := quant.?
			end = compile_closure(nfa, start, end, q.min, q.max)
		}
	}
	return
}

// Concat+Rune
compile_rune :: proc(nfa: ^NFA, r: rune, start: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.println("compile_rune", start)
	end = add_state(nfa)
	add_transition(nfa, start, end, r)
	// fmt.println("compile_rune: start, end", start, end)
	return
}
compile_wildcard :: proc(nfa: ^NFA, start: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	end = add_state(nfa)
	add_transition(nfa, start, end, Rune_Set{ invert = true, runes = nil })
	return
}

compile_closure :: proc(nfa: ^NFA, start: int, current_end: int, _min: int, _max: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// fmt.printf("compile_closure: (%v,%v),[%v,%v]\n", start, current_end, _min, _max)
	end = current_end
	local_end := current_end
	// Min-Repeats:
	for i in 1 ..< _min {
		ce := repeat_fragment(nfa, {start, current_end}, local_end)
		local_end = ce
	}
	// predict end, not thread safe, could do temp alloc []int for "Bypass Further Reps" transition
	end = max(local_end, local_end + _max - _min)
	if _min == 0 && _max == 1 {end -= 1} 	// edge case for '?'

	// Max-Repeats:
	for i in max(1, _min) ..< _max {
		ce := repeat_fragment(nfa, {start, current_end}, local_end)
		add_transition(nfa, local_end, end, ɛ) // Bypass Further Reps
		local_end = ce
	}
	assert(end == local_end, "Bad End")
	if _min == 0 {
		// Bypass:
		add_transition(nfa, start, end, ɛ)
	}
	if _max == -1 {
		// Loopback (*, +, {n,})
		add_transition(nfa, end, start, ɛ)
	}
	return
}

repeat_fragment :: proc(nfa: ^NFA, to_clone: Automata, start_at: int) -> (cloned_end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	state_mapping := make(map[int]int);defer delete(state_mapping)
	state_mapping[to_clone.start] = start_at

	// Clone states
	reachable_states := move_to_end(nfa, to_clone.start, to_clone.end);defer set.destroy(&reachable_states)
	assert(set.contains(&reachable_states, to_clone.end), "clone_fragment: start not connected to end")
	// Add cloned states to the state_mapping
	for state in reachable_states.m {
		if state == to_clone.start {continue}
		state_mapping[state] = add_state(nfa)
	}
	// Clone transitions
	for state, cloned in state_mapping {
		state_transitions := nfa.transitions[state]
		for transition in state_transitions {
			if transition.to not_in state_mapping {continue}
			new_to := state_mapping[transition.to]
			// fmt.printf("add_transition(%v,%v,%v)\n", cloned, new_to, transition.match)
			add_transition(nfa, cloned, new_to, transition.match)
		}
	}
	cloned_end = state_mapping[to_clone.end]
	return
}

// Traverse the nfa until it reaches `end` (inclusive).
move_to_end :: proc(nfa: ^NFA, start: int, end: int) -> Set(int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	s := set.init(int)

	seen := make([]bool, len(nfa.transitions))
	seen[start] = true
	defer delete(seen)
	stack := [dynamic]int{start}

	for len(stack) > 0 {
		state := pop(&stack)
		for transition in &nfa.transitions[state] {
			if !seen[transition.to] {
				seen[transition.to] = true
				append(&stack, transition.to)
			}
		}
	}

	for was_seen, state in seen {
		if was_seen {
			set.add(&s, state)
		}
	}

	return s
}
WHITESPACE := []rune{' ', '\t', '\v', '\n', '\r', '\f'}
compile_charset :: proc(nfa: ^NFA, charset: Charset, start: int) -> (end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)

	runeset := make([dynamic]Rune_Set_Item)

	for specifier in charset.specifiers {
		switch s in specifier {
		case Literal:
			append(&runeset, s)
		case Range:
			append(&runeset, Rune_Range{ rune(s.min), rune(s.max) })
		case EscapeSequence:
			switch esc in s {
			case RegexCommand:
				switch esc {
				case .Any_Digit:
					append(&runeset, Rune_Class.Digit)
				case .Not_Digit:
					append(&runeset, Rune_Class.Not_Digit)
				case .Any_Whitespace:
					append(&runeset, Rune_Class.Space)
				case .Not_Whitespace:
					append(&runeset, Rune_Class.Not_Space)
				case .Any_Word:
					append(&runeset, Rune_Class.Word)
				case .Not_Word:
					append(&runeset, Rune_Class.Not_Word)
				case .Carriage_Return:
					append(&runeset, '\r')
				case .Newline:
					append(&runeset, '\n')
				case .Not_Newline:
					append(&runeset, Rune_Class.Not_Newline)
				case .Tab:
					append(&runeset, '\t')
				case .Invalid:
					panic("INVALID REGEX COMMAND")
				}
			case rune:
				append(&runeset, esc)
			case ControlChar:
				unimplemented()
			case OctalCode:
				append(&runeset, rune(esc))
			case Codepoint:
				append(&runeset, rune(esc))
			case PropertyEscape:
				unimplemented()
			}
		}
	}
	shrink(&runeset)
	end = add_state(nfa)
	add_transition(nfa, start, end, Rune_Set{ invert = charset.caret, runes = runeset[:] })
	return
}

add_transition :: proc(nfa: ^NFA, from: int, to: int, match: MatchKind, group: int = 0) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	transition := Transition{to, group, match}
	transitions := &nfa.transitions[from]
	// for t in transitions {if t == transition {return}}
	append(transitions, transition)

	// Maintain the invariant of having all ɛ-transitions at the beginning.
	// it's possible to make this faster by finding the target position via binary search,
	// but for small-ish arrays this is plenty fast.
	for i := len(transitions) - 1; i > 0 && is_epsilon(transitions[i - 1].match); i -= 1 {
		transitions[i - 1], transitions[i] = transitions[i], transitions[i - 1]
	}
}

is_epsilon :: proc(match: MatchKind) -> bool {
	_, is_eps := match.(Epsilon)
	return is_eps
}
