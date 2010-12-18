open BatStd

type handle = int

type 'a db_entry =
	{handle: handle; data: 'a}
	
let get_entrydata entry = 
	entry.data
	
let xml_of_dbentry xml_of_a db_entry =
	let entryxml = xml_of_a db_entry.data in
	Xml.Element (
		Xml.tag entryxml,
		("handle",string_of_int db_entry.handle) :: 
			Xml.attribs entryxml,
		Xml.children entryxml
	)

type todolist = Todo.t db_entry list
type closedlist = Todo.closed_t db_entry list

let xml_of_todolist tdlist =
	Xml.Element ("todolist",[],(List.map (xml_of_dbentry Todo.xml_of_t) tdlist))

type db_info = {mutable last_id : int}

type t = 
	{
	 mutable open_todos : todolist;
	 mutable closed_todos : Todo.closed_t db_entry Daterange.t;
	 info : db_info}
  
type t_ref = t Persistence.t

(** Creating, loading and storing  databases **)

let make_database () = {open_todos = []; 
                        closed_todos = Daterange.empty;
						info = {last_id = 0}}

let load () = 
  Persistence.create "Tododatabase" (make_database ())
  
		
(** Database Access **)

let is_active db entry date =
	  let todo = entry.data in
  	let range = Todo.get_active_time todo date in
  	if Daterange.in_range date range then
  		match todo.Todo.duetime with
  		| Todo.Single tododate -> true
  		| Todo.Repeated _ -> 
  				let closed_todos = Daterange.find_range db.closed_todos range in
  				let predicate closed_todo = 
  					closed_todo.handle = entry.handle
  				in
  				not (List.exists predicate closed_todos)
  	else
  		false
 	
	
let get_active db_ref date =
  Persistence.access db_ref
  	(fun db ->	List.filter (fun todo -> is_active db todo date) db.open_todos )
	
let get_open db_ref =
	Persistence.access db_ref 
	   (fun db -> db.open_todos)
	

(** Database manipulations **)

let get_id db =
	let id = db.info.last_id in
	db.info.last_id <- db.info.last_id +1;
	id

let add           todo db = db.open_todos   <- todo :: db.open_todos

let add_closed todo db handle = 
	let date = Todo.get_closedate todo in
	db.closed_todos <- Daterange.add db.closed_todos date {data=todo;handle=handle}
	
let delete        todo db = db.open_todos <- List.filter (fun x -> x != todo) db.open_todos

(*
(* TODO: Rework using db_entries and handles *)

let close todo db date =
	let closed = Todo.close todo date in
	add_closed closed db;
	if not (Todo.is_repeated todo) then
		delete todo db
*)

let make_todo db name duetime  =
	let id = get_id db in
	let todo = Todo.make_open name duetime in
	add {handle = id; data = todo} db
	
(** Operations with todos **)	


              
         