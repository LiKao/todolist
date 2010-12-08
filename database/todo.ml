(** Types **)

type repetition = 
	| Daily
	| Weekly of Date.weekday
	| Monthly of Date.dayofmonth
	| Weekdays
	| Weekends

type todotime = 
	| Repeated of repetition
 	| Single   of Date.date

	
type t = 
	{duetime : todotime;
   subject : string;
	 id : int}
	
type close_state = 
	| Closed     of Date.date
	| Unfinished of Date.date
	
type closed_t =
	{todo  : t;
	 state : close_state}

(** Conversion from and to xml**)
let xml_of_repetition repetition =
	match repetition with
		Daily -> 
			"<dayly/>"
	| Weekly weekday ->
			Printf.sprintf 
				"<weekly>%s</weekly>"
				(Date.xml_of_weekday weekday)
	| Monthly dayofmonth ->
			Printf.sprintf
				"<monthly>%i</monthly>"
				dayofmonth
	| Weekdays ->
				"<Weekdays/>" 
	| Weekends ->
			"<Weekends/>"
		

let xml_of_duetime duetime =
	match duetime with
		Repeated repetition ->
			Printf.sprintf 
				"<repeated>%s</repeated>"
				(xml_of_repetition repetition)
	|  Single date ->
			Printf.sprintf
			"<single>%s</single>"
			(Date.xml_of_date date)
		

let xml_of_t todo =
		Printf.sprintf 
			"<todo id=\"%i\">  
			   <subject>%s</subject>
			   <duetime>%s</duetime>
			 </todo>
				"
			todo.id 
			todo.subject 
			(xml_of_duetime todo.duetime)
			
(** Queries on Todos **)

let is_repeated todo =
	match todo.duetime with
	| Repeated _ -> true
	| Single   _ -> false

let get_closedate closed_todo =
	match closed_todo.state with
		| Closed date -> date
		| Unfinished date -> date

let get_active_repetition repetition date =
	match repetition with
	| Daily -> Daterange.At date
	| Weekly weekday -> 
			let start = Date.previous_weekdate date weekday in
			let finish = Date.next_weekdate date weekday in
			Daterange.Between {Daterange.start = start; Daterange.finish = finish}
	| Monthly dayofmonth -> 
			let start  = Date.previous_monthdate date dayofmonth in
			let finish = Date.next_monthdate date dayofmonth in
			Daterange.Between {Daterange.start = start; Daterange.finish = finish}
	| Weekdays -> 
			let weekday = Date.get_weekday date in
			if Date.is_weekday weekday then
				Daterange.At date
			else
				Daterange.Nothing
	| Weekends ->
			let weekday = Date.get_weekday date in
			if Date.is_weekend weekday then
				Daterange.At date
			else
				Daterange.Nothing
	
let get_active_time todo date =
	match todo.duetime with
		| Repeated repetition -> get_active_repetition repetition date
		| Single date -> Daterange.Before date	

let get_duetime todo =
	todo.duetime
	
let get_subject todo =
	todo.subject

let get_id todo =
	todo.id

(** Todo manipulations **)

let make_open name duetime id = {duetime = duetime;subject=name;id=id}
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