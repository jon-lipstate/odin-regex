package regex
//
import "core:fmt"
import ba "core:container/bit_array"

// TODO (Jon/Andreas): After basic impl, refactor to a single/backing array, groups are sliced into it.
// Indexing: match_index * group_index

// Matches :: struct {
//     matches: []Match,
//     _matrix: Matrix(Span),
// }
// Matrix :: struct(T:typeid) {
//     data: [^]T,
//     rows, cols: int,
// }
// make_matches :: proc () -> (result: Matches) {
//     result._matrix.data = make([^]Span, num_matches * num_groups)
//     result._matrix.rows = num_groups
//     result._matrix.cols = num_matches

//     result.matches = make([]Match, num_matches)

//     for match, i in &result.matches {
//         match = Match{
//             span   = result._matrix.data[i * num_groups],
//             groups = result._matrix.data[i * num_groups + 1: i * num_groups + num_groups]
//         }
//     }

//     return
// }

Span :: struct {
	begin, end: int,
}
Match :: struct {
	using span: Span,
	groups:     []Span,
}

// TODO (jon): NO RUNES - ONLY u8??
match_transition :: proc(m: MatchKind, input: rune) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	result := false
	switch value in m {
	case Epsilon:
		result = input == 0
	case rune:
		result = input == value
	case ^Bit_Array:
		assert(input < 256)
		result = test_bit_unchecked(value, int(input))
	case Anchor_Start:
		unimplemented()
	case Anchor_End:
		unimplemented()
	}
	return result
}
// TODO(jon): remove dynamic on return
match :: proc(nfa: ^NFA, input: string) -> (matches: [dynamic]Match, found_any: bool) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	@(static)
	states_a, states_b: ba.Bit_Array
	reserve_unchecked(&states_a, len(nfa.transitions))
	defer ba.clear(&states_a)
	reserve_unchecked(&states_b, len(nfa.transitions))
	defer ba.clear(&states_b)

	current_states := &states_a
	next_states := &states_b

	@(static)
	group_stack: [dynamic]int // Char-Index on Entering a Group

	matches = [dynamic]Match{}
	append(&matches, Match{groups = make([]Span, nfa.next_group_id - 1)})
	current_match_index := 0
	current_match := &matches[current_match_index]
	current_match.span = Span{1 << 63 - 1, -1}
	found_a_match := false

	strlen := len(input)

	update_active_states(nfa, current_states, nfa.start)
	// Iterate through input characters
	for r, i in input {
		ba.clear(next_states)
		when false {
			//Debug Char Iteration Print
			fmt.printf("(%v,%v) Active: ", r, i)
			for x := 0; x < len(nfa.transitions); x += 1 {
				if test_bit_unchecked(current_states, x) {fmt.printf("%v ", x)}
			}
			fmt.printf("\n")
		}

		it := ba.make_iterator(current_states)
		for state in ba.iterate_by_set(&it) {
			for t in nfa.transitions[state] {
				if test_bit_unchecked(next_states, t.to) {continue}
				if match_transition(t.match, r) {
					update_active_states(nfa, next_states, t.to)
					set_bit_unchecked(next_states, t.to)

					current_match.begin = min(current_match.begin, i)
					current_match.end = max(current_match.end, i + 1)

					if t.group != 0 {
						// if t.group < 0 {
						// 	append(&group_stack, i)
						// } else {
						// fmt.println(r, i, t)
						// slice_start := pop(&group_stack)
						// Invariant: Group_Id starts at 1, id-1 for zero-indexing:
						// current_match.groups[t.group - 1] = Span{slice_start, i + 1}
						// }
					}

				}
			}
		} //for state-iter
		// swap pointers:
		current_states, next_states = next_states, current_states

		at_end_state := test_bit_unchecked(current_states, nfa.end)
		if at_end_state {
			// fmt.println("at_end_state")
			found_a_match = true
			found_any = true
			if !nfa.global {
				// fmt.println("!nfa.global")
				break
			}
		} else if !at_end_state && found_a_match {
			// NOTE: this allows breaking out from invalid positions, need to rethink this entirely.
			// test case demo'ing the failure: regex:"a(bc)+", str:"abcbc",
			// fmt.println("!at_end_state && found_a_match")
			if nfa.global {
				// fmt.println("nfa.global")
				append(&matches, Match{groups = make([]Span, nfa.next_group_id - 1)})
				current_match_index += 1
				current_match = &matches[current_match_index]
				current_match.span = Span{1 << 63 - 1, -1}
				found_a_match = false
				update_active_states(nfa, current_states, nfa.start)
			} else {
				break // halt at first match
			}
		}
	}
	// if nfa.global && !test_bit_unchecked(current_states, nfa.end) {
	// 	// we preemptively allocate a group after last one closed, so need to discard
	// 	nfg := pop(&matches)
	// 	delete(nfg.groups)
	// }
	// Check if the final state is active
	found_any = test_bit_unchecked(current_states, nfa.end)
	if !found_any {
		for m in matches {delete(m.groups)}
		delete(matches)
		matches = nil
	}
	return
}

update_active_states :: proc(nfa: ^NFA, active_states: ^Bit_Array, state: int) #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	@(static)
	stack: [dynamic]int
	append(&stack, state)
	defer clear(&stack)

	for len(stack) > 0 {
		state := pop(&stack)
		if test_bit_unchecked(active_states, state) {continue}
		set_bit_unchecked(active_states, state)
		for t in nfa.transitions[state] {
			if t.match != ɛ {break}
			if !test_bit_unchecked(active_states, t.to) {
				set_bit_unchecked(active_states, t.to)
				append(&stack, t.to)
			}
		}
	}
}
