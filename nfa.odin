package regex
//
import "core:fmt"
import "./set"
Set :: set.Set
//
Automata :: struct {
	start: int,
	end:   int,
}
NFA :: struct {
	transitions: [dynamic]Transitions,
	start:       int,
	end:         int,
}
Transitions :: [dynamic]Transition
Transition :: struct {
	to:    int,
	match: MatchKind,
}
Epsilon :: struct {}
ɛ :: Epsilon{} // const
MatchKind :: union {
	Epsilon,
	rune,
	InvertibleRange,
	Anchor_Start,
	Anchor_End,
}
make_nfa :: proc(allocator := context.allocator) -> NFA {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	context.allocator = allocator
	nfa := NFA {
		transitions = make([dynamic]Transitions),
		start       = -1,
		end         = -1,
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
compile_expr :: proc(
	nfa: ^NFA,
	expr: Expr,
	start: int,
	allocator := context.allocator,
) -> (
	end: int,
) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// // fmt.println("compile_expr", start)
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
		case (Group):
			end = compile_expr(nfa, Expr(atom), start)
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
	range := InvertibleRange{-(1 << 32), 1 << 32, false}
	add_transition(nfa, start, end, range)
	return
}

compile_closure :: proc(
	nfa: ^NFA,
	start: int,
	current_end: int,
	_min: int,
	_max: int,
) -> (
	end: int,
) {
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
	reachable_states := move_to_end(
		nfa,
		to_clone.start,
		to_clone.end,
	);defer set.destroy(&reachable_states)
	assert(
		set.contains(&reachable_states, to_clone.end),
		"clone_fragment: start not connected to end",
	)
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
	// TODO: this whole procedure is not good for perf, need to rework
	ranges := make([dynamic]InvertibleRange, 0, len(charset.specifiers));defer delete(ranges)
	for specifier in charset.specifiers {
		switch s in specifier {
		case Literal:
			append(&ranges, InvertibleRange{int(s), int(s), charset.caret})
		case Range:
			append(&ranges, InvertibleRange{s.min, s.max, charset.caret})
		case EscapeSequence:
			switch esc in s {
			case RegexCommand:
				switch esc {
				case .Any_Digit:
					append(&ranges, InvertibleRange{'0', '9', charset.caret}) // \d
				case .Not_Digit:
					append(&ranges, InvertibleRange{'0', '9', !charset.caret}) // \D
				case .Any_Whitespace:
					// TODO: this is dumb, need better method
					for w in WHITESPACE {
						append(&ranges, InvertibleRange{int(w), int(w), charset.caret}) // \s
					}
				case .Not_Whitespace:
					// TODO: this is dumb, need better method
					for w in WHITESPACE {
						append(&ranges, InvertibleRange{int(w), int(w), !charset.caret}) // \S
					}
				case .Any_Word:
					// \w
					append(&ranges, InvertibleRange{int('a'), int('z'), charset.caret})
					append(&ranges, InvertibleRange{int('A'), int('Z'), charset.caret})
					append(&ranges, InvertibleRange{int('0'), int('9'), charset.caret})
					append(&ranges, InvertibleRange{int('_'), int('_'), charset.caret})
				case .Not_Word:
					// \W
					append(&ranges, InvertibleRange{int('a'), int('z'), !charset.caret})
					append(&ranges, InvertibleRange{int('A'), int('Z'), !charset.caret})
					append(&ranges, InvertibleRange{int('0'), int('9'), !charset.caret})
					append(&ranges, InvertibleRange{int('_'), int('_'), !charset.caret})
				case .Carriage_Return:
					append(&ranges, InvertibleRange{int('\r'), int('\r'), charset.caret})
				case .Newline:
					append(&ranges, InvertibleRange{int('\n'), int('\n'), charset.caret})
				case .Not_Newline:
					append(&ranges, InvertibleRange{int('\n'), int('\n'), !charset.caret})
				case .Tab:
					append(&ranges, InvertibleRange{int('\t'), int('\t'), charset.caret})
				case .Invalid:
					panic("INVALID REGEX COMMAND")
				}
			case rune:
				panic("invalid_codepath")
			case ControlChar:
				unimplemented()
			case OctalCode:
				append(&ranges, InvertibleRange{int(esc), int(esc), false})
			case Codepoint:
				append(&ranges, InvertibleRange{int(esc), int(esc), false})
			case PropertyEscape:
				unimplemented()
			}
		}
	}
	end = add_state(nfa)

	if len(ranges) == 1 {
		add_transition(nfa, start, end, ranges[0])
	} else {
		for rng in ranges {
			add_transition(nfa, start, end, rng)
		}
	}

	return
}

add_transition :: proc(nfa: ^NFA, from: int, to: int, match: MatchKind) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	transition := Transition{to, match}
	// if to == 3 && match == ɛ {
	// 	fmt.println("HI")
	// }
	for t in nfa.transitions[from] {if t == transition {return}}
	append(&nfa.transitions[from], transition)
}

match_transition :: proc(m: MatchKind, input: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	result := false
	switch value in m {
	case Epsilon:
		result = true
	case rune:
		result = input == value
	case InvertibleRange:
		contained := value.min <= int(input) && int(input) <= value.max
		result = value.invert ? !contained : contained
	case Anchor_Start:
		unimplemented()
	case Anchor_End:
		unimplemented()
	}
	return result
}


match :: proc(nfa: ^NFA, input: string) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// TODO: these could be replaced with `core:container/bit_array` for better performance on
	// sparse sets of states.
	states_a := make([]bool, len(nfa.transitions))
	defer delete(states_a)
	states_b := make([]bool, len(nfa.transitions))
	defer delete(states_b)

	current_states := states_a
	next_states := states_b

	update_active_states :: proc(nfa: ^NFA, active_states: []bool, state: int, r: rune) #no_bounds_check {
		TRACE(&spall_ctx, &spall_buffer, #procedure)

		@static stack: [dynamic]int
		append(&stack, state)
		defer clear(&stack)

		for len(stack) > 0 {
			state := pop(&stack)
			if active_states[state] { continue }
			active_states[state] = true
			for t in nfa.transitions[state] {
				if !active_states[t.to] && match_transition(t.match, r) {
					active_states[t.to] = true
					append(&stack, t.to)
				}
			}
		}
	}

	strlen := len(input)
	update_active_states(nfa, current_states, nfa.start, 0)
	// Iterate through input characters
	for r, i in input {
		for b in &next_states {b = false}
		for is_active, state in current_states {
			if !is_active {continue}
			for t in nfa.transitions[state] {
				if match_transition(t.match, r) {
					update_active_states(nfa, next_states, t.to, r)
					next_states[t.to] = true
				}
			}
		}
		// swap pointers:
		current_states, next_states = next_states, current_states
	}
	// fmt.println("current_states", current_states.m)
	// fmt.println("next_states", next_states.m)
	// Check if the final state is active
	for is_active, state in current_states {if is_active && state == nfa.end {return true}}
	return false
}

epsilon_closure :: proc(nfa: ^NFA, start: int) -> [dynamic]int {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	closure := make([dynamic]int)

	stack := [dynamic]int{start}
	defer delete(stack)

	seen := make([]bool, len(nfa.transitions))
	seen[start] = true
	defer delete(seen)

	for len(stack) > 0 {
		state := pop(&stack)
		append(&closure, state)
		for t in &nfa.transitions[state] {
			if !seen[t.to] && match_transition(t.match, 0) {
				seen[t.to] = true
				append(&stack, t.to)
			}
		}
	}

	return closure
}
reachable :: proc(transitions: [dynamic]Transitions, start: int) -> [dynamic]int {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	closure := make([dynamic]int)

	stack := [dynamic]int{start}
	defer delete(stack)

	seen := make([]bool, len(transitions))
	seen[start] = true
	defer delete(seen)

	for len(stack) > 0 {
		state := pop(&stack)
		append(&closure, state)
		for t in transitions[state] {
			if !seen[t.to] {
				seen[t.to] = true
				append(&stack, t.to)
			}
		}
	}

	return closure
}
eliminate_epsilons :: proc(nfa: ^NFA) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	new_transitions := make([dynamic]Transitions, len(nfa.transitions))
	for transitions, state in &nfa.transitions {
		if len(transitions) == 0 { continue }
		new_transitions[state] = make([dynamic]Transition, 0, len(transitions))
		closure := epsilon_closure(nfa, state)
		defer delete(closure)
		final_state_in_closure := len(closure) > 0 ? closure[len(closure) - 1] == nfa.end : false
		for next_state in closure {
			for t in nfa.transitions[next_state] {
				if !match_transition(t.match, 0) {
					append(&new_transitions[state], t)
				} else if final_state_in_closure {
					@(static)
					once := false
					if !once {
						append(&new_transitions[state], Transition{to = nfa.end, match = ɛ})
						once = true
					}
				}
			}
		}
	}
	// Copy over the clean paths:
	for v in &nfa.transitions {delete(v)}
	clear(&nfa.transitions)
	reach := reachable(new_transitions, nfa.start);defer delete(reach)
	for r in reach {nfa.transitions[r] = new_transitions[r]}
	delete(new_transitions)
}


// delete//??
clone_fragment :: proc(nfa: ^NFA, start: int, end: int) -> (cloned_start, cloned_end: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	state_mapping := make(map[int]int);defer delete(state_mapping)
	cloned_start = add_state(nfa)
	state_mapping[start] = cloned_start

	// Clone states
	reachable_states := move_to_end(nfa, start, end);defer set.destroy(&reachable_states)
	assert(set.contains(&reachable_states, end), "clone_fragment: start not connected to end")
	// Add cloned states to the state_mapping
	for state in reachable_states.m {
		if state == start {continue}
		state_mapping[state] = add_state(nfa)
	}
	// Clone transitions
	for state, cloned in state_mapping {
		state_transitions := nfa.transitions[state]
		for transition in state_transitions {
			new_to := state_mapping[transition.to]
			add_transition(nfa, cloned, new_to, transition.match)
		}
	}
	cloned_end = state_mapping[end]
	return
}
