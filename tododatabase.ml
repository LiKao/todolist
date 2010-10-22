TYPE_CONV_PATH "Current.Module.Name"

open Sexplib
																			
type todolist = Todo.t list with sexp

type t = 
	{mutable open_todos : todolist;
	 mutable closed_todos : todolist}
	with sexp

(** Creating, loading and storing  databases **)

let make_database () = {open_todos = []; closed_todos = []}

let load prefix =
	let open_todo_filename = Printf.sprintf "%s.open.sexp" prefix in
	let closed_todo_filename = Printf.sprintf "%s.closed.sexp" prefix in
	let read_file filename =
		try
			ignore (Unix.stat filename);
			let sexp = Sexp.load_sexp filename in
			todolist_of_sexp sexp 
		with Unix.Unix_error _ ->
			[]
	in
		{open_todos = read_file open_todo_filename;
		 closed_todos = read_file closed_todo_filename}
		
let store prefix db =
	let open_todo_filename = Printf.sprintf "%s.open.sexp" prefix in
	let closed_todo_filename = Printf.sprintf "%s.closed.sexp" prefix in
	let write_file filename todolist =
		let sexp = sexp_of_todolist todolist in
		let outchan = open_out filename in
		Sexp.output_hum outchan sexp;
		close_out outchan
	in
	write_file open_todo_filename db.open_todos;
	write_file closed_todo_filename db.closed_todos

(** Database manipulations **)

let add           todo db = db.open_todos   <- todo :: db.open_todos
let add_closed    todo db = db.closed_todos <- todo :: db.closed_todos
let delete        todo db = db.open_todos <- List.filter (fun x -> x != todo) db.open_todos

(** Operations with todos **)
	
let choose todos = 
	let menu = Interaction.choices_of_list todos Todo.string_of_todo "Todo Auswählen" in
	Interaction.display_choice menu
	


              
         