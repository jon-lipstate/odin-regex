package regex
import ba "core:container/bit_array"
Bit_Array :: ba.Bit_Array
set_bit_unchecked :: #force_inline proc(b: ^Bit_Array, bit: int) #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	// bit>>6: div 64, bit&63: bit%63
	b.bits[bit >> 6] |= 1 << uint(bit & 63)
}
unset_bit_unchecked :: #force_inline proc(b: ^Bit_Array, bit: int) #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	b.bits[bit >> 6] &= ~(1 << uint(bit & 63))
}
set_bit_range :: #force_inline proc(b: ^Bit_Array, r: Range) #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	for i := r.min; i <= r.max; i += 1 {
		set_bit_unchecked(b, i)
	}
}
set_bit_range_inverted :: #force_inline proc(b: ^Bit_Array, r: Range) #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	for i := 0; i <= b.max_index; i += 1 {
		if i < r.min || i > r.max {
			set_bit_unchecked(b, i) // TODO(Jon): i think this may be wrong, should really do set ~i
		}
	}
}
test_bit_unchecked :: #force_inline proc(b: ^Bit_Array, index: int) -> bool #no_bounds_check {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	return bool((b.bits[index >> 6] >> uint(index & 63)) & 1)
}
reserve_unchecked :: proc(ba: ^Bit_Array, size: int) {
	TRACE(&spall_ctx, &spall_buffer, #procedure)
	if size > ba.max_index + 1 {
		ba.max_index = size - 1
		resize(&ba.bits, 1 + (ba.max_index >> 6))
	}
}
