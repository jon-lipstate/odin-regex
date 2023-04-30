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
	defer if !found_any {
		for m in matches {delete(m.groups)}
		delete(matches)
		matches = nil
	}
	current_match := 0
	append(&matches, Match{groups = make([]Span, nfa.next_group_id - 1)})

	strlen := len(input)
	update_active_states(nfa, current_states, nfa.start)
	// Iterate through input characters
	for r, i in input {
		ba.clear(next_states)
		it := ba.make_iterator(current_states)
		for state in ba.iterate_by_set(&it) {
			for t in nfa.transitions[state] {
				if test_bit_unchecked(next_states, t.to) {continue}
				if match_transition(t.match, r) {
					update_active_states(nfa, next_states, t.to)
					set_bit_unchecked(next_states, t.to)
					if t.group != 0 {
						if t.group < 0 {
							append(&group_stack, i)
						} else {
							slice_start := pop(&group_stack)
							// Invariant: Group_Id starts at 1, id-1 for zero-indexing:
							matches[current_match].groups[t.group - 1] = Span{slice_start, i + 1}
						}
					}
				}
			}
		}
		// swap pointers:
		current_states, next_states = next_states, current_states
	}
	// fmt.println("current_states", current_states.m)
	// fmt.println("next_states", next_states.m)
	// Check if the final state is active
	found_any = test_bit_unchecked(current_states, nfa.end)
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
