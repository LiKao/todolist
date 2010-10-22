TYPE_CONV_PATH "Current.Module.Name"

open Sexplib

(** Types **)

type repetition = 
	| Daily
	| Weekly of Date.weekday
	| Monthly of Date.dayofmonth
	| Weekdays
	| Weekends
	with sexp

type todotime = 
	| Repeated of repetition
  | Single   of Date.date
	with sexp
	
type todostate = 
	| Pending
	| Closed     of Date.date
	| Unfinished of Date.date
	with sexp
	
type tododata = 
	{duetime : todotime;
   subject : string;
	 state   : todostate}
	with sexp

																								
type todolist = tododata list with sexp

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

(** Queries on Todos **)

let is_open todo =
	match todo.state with
	| Pending       -> true
	| Closed      _ -> false
	| Unfinished  _ -> false

let is_repeated todo =
	match todo.duetime with
	| Repeated _ -> true
	| Single   _ -> false

(** Todo manipulations **)

let make_open duetime name = {duetime = duetime;subject=name;state=Pending}
let close todo date = {todo with state=Closed date}
let drop  todo date = {todo with state=Unfinished date}

(** conversion functions **)

let string_of_repetition =
	function
		Daily -> "Täglich"
	| Weekly weekday -> Printf.sprintf "jeden %s" (Date.string_of_weekday weekday)
	| Monthly day    -> Printf.sprintf "jeden %i." day
	| Weekdays       -> "wochentags"
	| Weekends       -> "am wochenende"

let string_of_todotime =
	function
		Repeated repetition -> string_of_repetition repetition 
  | Single   date       -> 
		  Printf.sprintf "bis %s" (if Date.is_today date then "heute" else Date.string_of_date date)

let string_of_todo todo =
	Printf.sprintf "%s\t %s" todo.subject (string_of_todotime todo.duetime)
	
(** Operations with todos **)
	
let choose todos = 
	let menu = Interaction.choices_of_list !todos string_of_todo "Todo Auswählen" in
	Interaction.display_choice menu
	


              
         