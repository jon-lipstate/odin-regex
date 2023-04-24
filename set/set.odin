package set
Set :: struct($T: typeid) {
	m: map[T]struct {},
}
// Allocates interior `map`
init :: #force_inline proc($T: typeid, allocator := context.allocator) -> Set(T) {
	s := Set(T){}
	s.m = make_map(map[T]struct {}, 16, allocator)
	return s
}
// Deallocates interior `map`
destroy :: #force_inline proc(s: ^Set($T)) {
	delete(s.m)
}
contains :: #force_inline proc(s: ^Set($T), item: T) -> bool {
	return item in s.m
}
// `if item in set` -> `false`
add :: #force_inline proc(s: ^Set($T), item: T) -> (ok: bool) {
	if item in s.m {return false} else {
		s.m[item] = {}
		return true
	}
}
// `if item not_in set` -> `false`
remove :: #force_inline proc(s: ^Set($T), item: T) -> (ok: bool) {
	if item not_in s.m {return false} else {
		delete_key(s.m, item)
		return true
	}
}
is_empty :: #force_inline proc(s: ^Set($T)) -> bool {
	return len(s.m) == 0
}
clear_set :: #force_inline proc(s: ^Set($T)) {
	clear(&s.m)
}
