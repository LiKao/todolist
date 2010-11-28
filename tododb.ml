type todolist = Todo.t list
type closedlist = Todo.closed_t list

type db_info = {mutable last_id : int}

type t = 
	{mutable open_todos : todolist;
	 mutable closed_todos : Todo.closed_t Daterange.t;
	 info : db_info}
  
type t_ref = t Persistence.t

(** Creating, loading and storing  databases **)

let make_database () = {open_todos = []; 
                        closed_todos = Daterange.empty;
						info = {last_id = 0}}

let load () = 
  Persistence.create "Tododatabase" (make_database ())
  
		
(** Database Access **)

let is_active db todo date =
  	let range = Todo.get_active_time todo date in
  	if Daterange.in_range date range then
  		match todo.Todo.duetime with
  		| Todo.Single tododate -> true
  		| Todo.Repeated _ -> 
  				let closed_todos = Daterange.find_range db.closed_todos range in
  				let predicate closed_todo = 
  					closed_todo.Todo.todo.Todo.id = todo.Todo.id
  				in
  				not (List.exists predicate closed_todos)
  	else
  		false
 	
	
let get_active db_ref date =
  Persistence.access db_ref
  	(fun db ->	List.filter (fun todo -> is_active db todo date) db.open_todos)
	
let get_open db_ref =
	Persistence.access db_ref 
	   (fun db -> db.open_todos)
	

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


              
         