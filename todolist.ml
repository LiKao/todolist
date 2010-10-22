open Printf

(* get path to home directory *)
let homedir = Unix.getenv "HOME"

let datadir = sprintf "%s/.todolist" homedir

let _ = 
	try
		ignore (Unix.stat datadir)
	with Unix.Unix_error _ ->
		Unix.mkdir datadir 0o777

let open_todo_file = sprintf "%s/open.sexp" datadir
let closed_todo_file = sprintf "%s/closed.sexp" datadir

let load filename = 
	try
		ignore (Unix.stat filename);
		let sexp = Sexplib.Sexp.load_sexp open_todo_file in
		ref (Tododatabase.todolist_of_sexp sexp) 
	with Unix.Unix_error _ ->
		Tododatabase.make_database ()
		
let store filename database =
	let sexp = Tododatabase.sexp_of_todolist database in
	let outchan = open_out filename in
	Sexplib.Sexp.output_hum outchan sexp

let open_todos = load open_todo_file

let closed_todos = load closed_todo_file

let _ =
  let rec main () =
		printf "Heute ist %s.\n" (Date.string_of_date (Date.get_today ()));
		let quit = (Interaction.display_menu (Inout.main_menu open_todos)) () in
		if not quit then
			main ()
		else
			()
		in
		main ()
		
let _ = store open_todo_file !open_todos
let _ = store closed_todo_file !closed_todos