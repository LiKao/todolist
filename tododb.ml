TYPE_CONV_PATH "Tododb"

open Sexplib																			
																																																									
type todolist = Todo.t list with sexp
type closedlist = Todo.closed_t list with sexp

type db_info = {mutable last_id : int} with sexp 

type t = 
	{mutable open_todos : todolist;
	 mutable closed_todos : Todo.closed_t Daterange.t;
	 info : db_info}

(** Creating, loading and storing  databases **)

let make_database () = {open_todos = []; 
                        closed_todos = Daterange.empty;
												info = {last_id = 0}}

let load prefix =
	let open_todo_filename = Printf.sprintf "%s.open.sexp" prefix in
	let closed_todo_filename = Printf.sprintf "%s.closed.sexp" prefix in
	let info_filename = Printf.sprintf "%s.info.sexp" prefix in
	let read_file filename prser default =
		try
			ignore (Unix.stat filename);
			let sexp = Sexp.load_sexp filename in
			prser sexp 
		with Unix.Unix_error _ ->
			default
	in
		let open_todos = read_file open_todo_filename todolist_of_sexp [] in
		let closed_todo_list = read_file closed_todo_filename closedlist_of_sexp [] in
		let db_info = read_file info_filename db_info_of_sexp {last_id = 0} in
		let closed_todos = Daterange.from_list Todo.get_closedate closed_todo_list in  
		{open_todos = open_todos;
		 closed_todos = closed_todos;
		 info = db_info}
		
let store prefix db =
	let open_todo_filename = Printf.sprintf "%s.open.sexp" prefix in
	let closed_todo_filename = Printf.sprintf "%s.closed.sexp" prefix in
	let info_filename = Printf.sprintf "%s.info.sexp" prefix in
	let write_file filename todolist writer =
		let sexp = writer todolist in
		let outchan = open_out filename in
		Sexp.output_hum outchan sexp;
		close_out outchan
	in
	let closed_todo_list = Daterange.to_list db.closed_todos in
	write_file open_todo_filename db.open_todos sexp_of_todolist;
	write_file closed_todo_filename closed_todo_list sexp_of_closedlist;
	write_file info_filename db.info sexp_of_db_info

(** Database manipulations **)

let get_id db =
	let id = db.info.last_id in
	db.info.last_id <- db.info.last_id +1;
	id

let add           todo db = db.open_todos   <- todo :: db.open_todos

let add_closed    todo db = 
	let date = Todo.get_closedate todo in
	db.closed_todos <- Daterange.add db.closed_todos date todo
	
let delete        todo db = db.open_todos <- List.filter (fun x -> x != todo) db.open_todos

let close todo db date =
	let closed = Todo.close todo date in
	add_closed closed db;
	if not (Todo.is_repeated todo) then
		delete todo db
		
let make_todo db name duetime  =
	let id = get_id db in
	let todo = Todo.make_open name duetime id in
	add todo db
	
(** Operations with todos **)
	
let choose todos = 
	let menu = Interaction.choices_of_list todos Todo.string_of_todo "Todo Auswählen" in
	Interaction.display_choice menu
	


              
         