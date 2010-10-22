TYPE_CONV_PATH "Current.Module.Name"

open Sexplib
																			
type todolist = Todo.t list with sexp

type database = todolist ref

(** Creating, loading and storing  databases **)

let make_database () = ref []

let load filename = 
	try
		ignore (Unix.stat filename);
		let sexp = Sexp.load_sexp filename in
		ref (todolist_of_sexp sexp) 
	with Unix.Unix_error _ ->
		make_database ()
		
let store filename database =
	let sexp = sexp_of_todolist database in
	let outchan = open_out filename in
	Sexp.output_hum outchan sexp

(** Database manipulations **)

let add todo database    = database := todo :: !database
let delete todo database = database := List.filter (fun x -> x != todo) !database

(** Operations with todos **)
	
let choose todos = 
	let menu = Interaction.choices_of_list !todos Todo.string_of_todo "Todo Auswählen" in
	Interaction.display_choice menu
	


              
         