open Printf

(* get path to home directory *)
let homedir = Unix.getenv "HOME"

let datadir = sprintf "%s/.todolist" homedir

let _ = 
	try
		ignore (Unix.stat datadir)
	with Unix.Unix_error _ ->
		Unix.mkdir datadir 0o777

let database_prefix = sprintf "%s/todo" datadir

let db = Tododatabase.load database_prefix

let _ =
  let rec main () =
		printf "Heute ist %s.\n" (Date.string_of_date (Date.get_today ()));
		let quit = (Interaction.display_menu (Inout.main_menu db)) () in
		if not quit then
			main ()
		else
			()
		in
		main ()
		
let _ = Tododatabase.store database_prefix db
