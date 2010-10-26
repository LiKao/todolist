module T =
struct
	type t = Date.date
	let compare = Date.compare
end

module S = Range.Make (T)

include S