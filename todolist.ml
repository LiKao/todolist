open Printf

let datadir = "/opt/godi/var/todolist/"

let _ = 
	try
		ignore (Unix.stat datadir)
	with Unix.Unix_error _ ->
		Unix.mkdir datadir 0o777

let database_prefix = sprintf "%s/todo" datadir

let db = Tododb.load database_prefix

let _ = Services.register_all db

let shutdown () = Tododb.store database_prefix db

let _ = Services.at_exit shutdown
