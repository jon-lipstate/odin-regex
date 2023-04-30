package regex
//
import "core:fmt"
import ba "core:container/bit_array"

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

match :: proc(nfa: ^NFA, input: string) -> bool {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	@(static)
	states_a, states_b: ba.Bit_Array
	reserve_unchecked(&states_a, len(nfa.transitions))
	defer ba.clear(&states_a)
	reserve_unchecked(&states_b, len(nfa.transitions))
	defer ba.clear(&states_b)

	current_states := &states_a
	next_states := &states_b

	update_active_states :: proc(nfa: ^NFA, active_states: ^Bit_Array, state: int) #no_bounds_check {
		TRACE(&spall_ctx, &spall_buffer, #procedure)
		@(static)
		stack: [dynamic]int
		append(&stack, state)
		defer clear(&stack)

		for len(stack) > 0 {
			state := pop(&stack)
			if test_bit_unchecked(active_states, state) {continue}
			ba.set(active_states, state)
			for t in nfa.transitions[state] {
				if t.match != (Epsilon{}) { break }
				if !test_bit_unchecked(active_states, t.to) {
					set_bit_unchecked(active_states, t.to)
					append(&stack, t.to)
				}
			}
		}
	}
	group_stacks: [dynamic]Pair(int)

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
							append(&group_stacks, Pair(int){abs(t.group), i})
							fmt.println("add to grp-stack", t.group, i)
						} else {
							grp := pop(&group_stacks)
							assert(grp.b == t.group)
							fmt.printf("Group %v [%v:%v], (%v)\n", t.group, grp.b, i + 1, input[grp.b:i + 1])
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
	if test_bit_unchecked(current_states, nfa.end) {return true}
	return false
}
