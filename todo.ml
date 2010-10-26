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
	
type t = 
	{duetime : todotime;
   subject : string}
	with sexp
	
type close_state = 
	| Closed     of Date.date
	| Unfinished of Date.date
	with sexp
	
type closed_t =
	{todo  : t;
	 state : close_state}
	with sexp
	
(** Queries on Todos **)

let is_repeated todo =
	match todo.duetime with
	| Repeated _ -> true
	| Single   _ -> false

let get_closedate closed_todo =
	match closed_todo.state with
		| Closed date -> date
		| Unfinished date -> date

(** Todo manipulations **)

let make_open duetime name = {duetime = duetime;subject=name}
let close todo date = {todo = todo; state = Closed date}
let drop  todo date = {todo = todo; state = Unfinished date}

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