TYPE_CONV_PATH "Current.Module.Name"

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
	
type t = 
	{duetime : todotime;
   subject : string;
	 state   : todostate}
	with sexp
	
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