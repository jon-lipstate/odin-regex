package ba

import "core:container/bit_array"

set_unchecked :: proc (ba: ^bit_array.Bit_Array, index: int) #no_bounds_check {
	ba.bits[index >> 6] |= 1 << uint(index & 63)
}

get_unchecked :: proc (ba: ^bit_array.Bit_Array, index: int) -> bool #no_bounds_check {
	return bool((ba.bits[index >> 6] >> uint(index & 63)) & 1)
}

reserve_unchecked :: proc (ba: ^bit_array.Bit_Array, size: int) {
	if size > ba.max_index + 1 {
		ba.max_index = size - 1
		resize(&ba.bits, 1 + (ba.max_index >> 6))
	}
}
