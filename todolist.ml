open Printf

let datadir = "/opt/godi/var/todolist/"

let _ = 
	try
		ignore (Unix.stat datadir)
	with Unix.Unix_error _ ->
		Unix.mkdir datadir 0o777

let database_prefix = sprintf "%s/todo" datadir

let db = Tododb.load database_prefix

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
		
let _ = Tododb.store database_prefix db
