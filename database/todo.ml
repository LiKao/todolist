open BatStd

(** Types **)

type repetition = 
	| Daily
	| Weekly of Date.weekday
	| Monthly of Date.dayofmonth
	| Weekdays
	| Weekends

let repetition_spec = 
	Xml.Element ("variant",[("fieldname","repetition")],[
		Xml.Element ("variant_entry",[
				("name","täglich");
				("fieldname","daily")
			],[
				(*empty*)
		]);
		Xml.Element ("variant_entry",[
				("name","wöchentlich");
				("fieldname","weekly")
			],[
				Date.weekday_spec	
			]);
		Xml.Element ("variant_entry",[
				("name","monatlich");
				("fieldname","monthly");
			],[
				Date.dayofmonth_spec
		]);
		Xml.Element ("variant_entry",[
				("name","wochentags");
				("fieldname","weekdays");
			],[
				(*empty*)
		]);
		Xml.Element ("variant_entry",[
				("name","am Wochenende");
				("fieldname","weekends")
			],[
				(*empty*)
		]);
	])

type todotime = 
	| Repeated of repetition
 	| Single   of Date.date

let todotime_spec =
	Xml.Element ("variant",[("fieldname","duetime")],[
		Xml.Element ("variant_entry",[
				("name","wiederholt");
				("fieldname","repeated")
			],[
				repetition_spec
		]);
		Xml.Element ("variant_entry",[
				("name","einzeln");
				("fieldname","single")
			],[
				Date.date_spec
		])
	])
		
type t = 
	{duetime : todotime;
   subject : string;
	 id : int}
	
let t_spec =
	Xml.Element ("record",[
			("fieldname","todo")
		],[
			Xml.Element ("record_field",[],[
				Xml.Element ("input",[
					("type","string");
					("fieldname","subject")
				],[])
			]);
			Xml.Element ("record_field",[],[
				todotime_spec
			])
	])
	
type close_state = 
	| Closed     of Date.date
	| Unfinished of Date.date
	
type closed_t =
	{todo  : t;
	 state : close_state}

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
	
(** Conversion from and to xml**)
let xml_of_repetition repetition =
	Xml.Element ("repetition",[],[
		match repetition with
			Daily -> Xml.Element ("daily",[],[])
		| Weekly weekday -> Xml.Element ("weekly",[],[Date.xml_of_weekday weekday])
		| Monthly dayofmonth -> Xml.Element ("monthly",[],[Xml.PCData (string_of_int dayofmonth)])
		| Weekdays -> Xml.Element ("weekdays",[],[]) 
		| Weekends -> Xml.Element ("weekends",[],[])
	])
		

let repetition_of_xml xml =
	Xmlhelpers.check_node 
		xml
		~name:"repetition"
		~node_error:"Todo.t_of_xml got non element node"
		~type_error:"Todo.t_of_xml got non-todo node";
	Xmlhelpers.get_single_child xml |>
	Xmlhelpers.produce
		["daily", 
			(fun _ -> Daily);
		 "weekly",
			(fun content ->
				Weekly ( 
					Xmlhelpers.get_single_child content |> 
					Date.weekday_of_xml
				)
			);
		"monthly",
			(fun content ->
				Monthly (
					Xmlhelpers.get_single_child content |>
					Date.day_of_xml
				)
			);
		"weekdays",
			(fun _ -> Weekdays);
		"weekends",
			(fun _ -> Weekends)]
			
let xml_of_duetime duetime =
	Xml.Element ("duetime",[],[
		match duetime with
			Repeated repetition -> Xml.Element ("repeated",[],[xml_of_repetition repetition])
		|  Single date -> Xml.Element ("single",[],[Date.xml_of_date date])
	])
	
let duetime_of_xml xml =
	Xmlhelpers.check_node 
		xml
		~name:"duetime"
		~node_error:"Todo.duetime_of_xml got non element node"
		~type_error:"Todo.duetime_of_xml got non duetime node";
	Xmlhelpers.get_single_child xml |>
	Xmlhelpers.produce 
		["repeated",
			(fun content ->
				Repeated ( 
					Xmlhelpers.get_single_child content |> 
					repetition_of_xml
				)
			);
		 "single",
			(fun content ->
				Single (
					Xmlhelpers.get_single_child content |> 
					Date.date_of_xml
				)
			)
		]
		
let xmltododata todo =
	[
		Xml.Element ("subject",[],[Xml.PCData todo.subject]);
		xml_of_duetime todo.duetime
	]

let xml_of_t todo =
	Xml.Element ("todo",
		[
			("id",string_of_int todo.id);
			("state","open")
		], 
		xmltododata todo)

let t_of_xml ?id xml=
	Xmlhelpers.check_node 
		xml
		~name:"todo"
		~node_error:"Todo.t_of_xml got non element node"
		~type_error:"Todo.t_of_xml got non-todo node";
	let subject =
		xml |>
		Xmlhelpers.find_child "subject" |>
		Xmlhelpers.access_text
	in
	let duetime =
		xml |>
		Xmlhelpers.find_child "duetime" |>
		duetime_of_xml
	in
	let id =
		match id with
			| Some id -> id
			| None -> 
				begin
					try
						Xml.attrib xml "id" |> int_of_string
					with 
						Xml.No_attribute _ -> 
							raise (Xmlhelpers.Parse_Error "Todo.t_of_xml: Xml is missing id attribure")
				end
	in
	{subject=subject;duetime=duetime;id=id}
		
let xml_of_closed_t closed =
	let state,date =
		match closed.state with
			Closed date -> ("closed",date)
		| Unfinished date -> ("unfinished",date)
	in
	Xml.Element ("todo",
		[
			("id",string_of_int closed.todo.id);
		  ("state",state)
		],
		[Date.xml_of_date date] 
		@ (xmltododata closed.todo))
			