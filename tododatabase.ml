TYPE_CONV_PATH "Current.Module.Name"

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
	
let is_repeated =
	function
		| Repeated _ -> true
		| Single   _ -> false
	
type todostate = 
	| Pending
	| Closed     of Date.date
	| Unfinished of Date.date
	with sexp
	
let is_open =
	function
		| Pending       -> true
		| Closed      _ -> false
		| Unfinished  _ -> false
							

type tododata = 
	{duetime : todotime;
   subject : string;
	 state   : todostate}
	with sexp

																								
type todolist = tododata list with sexp

type database = todolist ref

let make_database () = ref []

let make_open_todo duetime name = {duetime = duetime;subject=name;state=Pending}

let add_todo todo database    = database := todo :: !database
let delete_todo todo database = List.filter (fun x -> x != todo) database

let close_todo todo date = {todo with state=Closed date}
let drop_todo  todo date = {todo with state=Unfinished date}

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


              
         