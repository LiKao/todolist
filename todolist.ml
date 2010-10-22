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



let open_todos = Tododatabase.load open_todo_file

let closed_todos = Tododatabase.load closed_todo_file

let _ =
  let rec main () =
		printf "Heute ist %s.\n" (Date.string_of_date (Date.get_today ()));
		let quit = (Interaction.display_menu (Inout.main_menu (open_todos,closed_todos))) () in
		if not quit then
			main ()
		else
			()
		in
		main ()
		
let _ = Tododatabase.store open_todo_file !open_todos
let _ = Tododatabase.store closed_todo_file !closed_todos