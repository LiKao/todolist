TYPE_CONV_PATH "Range"

open Sexplib

module type Rangetype =
sig
	type t
	val compare : t -> t -> int
end
	
module type S =
sig
	type key
	type 'a t
	type interval = {start : key; finish : key} 
	type range = 
		| Nothing
		| At      of key
		| Before  of key
		| After   of key
		| Between of interval 
	val in_interval : key -> interval -> bool
	val in_range : key -> range -> bool
	val empty   : 'a t
	val add     : 'a t -> key -> 'a -> 'a t
	val find    : 'a t -> key -> 'a list
	val find_before : 'a t -> key -> 'a list
	val find_after  : 'a t -> key -> 'a list
	val find_between  : 'a t -> interval -> 'a list
	val find_range  : 'a t -> range -> 'a list
	val to_list : 'a t -> 'a list
	val from_list : ('a -> key) -> 'a list -> 'a t
end
	
module Make (Data : Rangetype) : (S with type key = Data.t) =
struct
	type key = Data.t
	type 'a leaf = {key : key; content : 'a}
	type 'a t = 'a leaf list 
	type interval = {start : key; finish : key}
	type range =
		| Nothing
		| At      of key
		| Before  of key
		| After   of key
		| Between of interval 
	
	let compare = Data.compare
	
	let empty = []
	
	let in_interval key interval =
		let b1 = compare key interval.start > 0 in
		b1 &&
			let b2 = compare key interval.finish <=0 in
			b2
		
	let in_range key =
		function 
		| Nothing          -> false
		| At      key2     -> compare key key2  = 0
		| Before  key2     -> compare key key2 <= 0
		| After   key2     -> compare key key2 >  0
		| Between interval -> in_interval key interval
 
	let add ls key value =
		let leaf = {key = key; content = value} in
		let rec loop =
			function
				node :: nodes ->
					if compare node.key key < 0 then
						leaf :: node :: nodes 
					else		
						node :: (loop nodes)
				| [] -> leaf :: []
		in
		loop ls
		
	let find ls key =
		let rec loop =
			function
				node :: nodes -> 
					let comparison = compare node.key key in
					if comparison = 0 then
						node.content :: (loop nodes)
					else if comparison > 0 then
						loop nodes
					else
						[]
			|  [] -> []
		in loop ls

	let find_before ls key =
		let rec loop =
			function
				node :: nodes ->
					let comparison = compare node.key key in
					if comparison <= 0 then
						List.map (fun leaf -> leaf.content) ls
					else
						loop nodes
			| [] -> []
		in loop ls
	
	let find_after ls key =
		let rec loop =
			function
				node :: nodes ->
					let comparison = compare node.key key in
					if comparison > 0 then
						node.content :: (loop nodes)
					else
						[]
			| [] -> []
		in
		loop ls		
						
	let find_between ls interval =
		let rec loop1 = 
			function
				node :: nodes ->
					let comparison = compare node.key interval.start in
					if comparison > 0 then
						node.content :: (loop1 nodes)
					else
						[]
			| [] -> []
		in
		let rec loop2 =
			function
				node :: nodes -> 
					let comparison = compare node.key interval.finish in
					if comparison <= 0 then
						loop1 (node :: nodes)
					else
						loop2 nodes
			| [] -> []
		in loop2 ls
	
	let find_range ls =
		function
		| Nothing          -> [] 
		| At      key      -> find ls key 
		|	Before  key      -> find_before ls key
		| After   key      -> find_after ls key
		| Between interval -> find_between ls interval 
		
	let to_list ls =
		List.map (fun leaf -> leaf.content) ls		
		
	let from_list get_key =
		List.fold_left 
			(fun ls value -> add ls (get_key value) value)
			empty	
end