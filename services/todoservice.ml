open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

let show_todos db date =
	let active_todos = Tododb.get_active db date in
	let printer todo =  
		let subject = Todo.get_subject todo in
		let duetime = Todo.string_of_todotime (Todo.get_duetime todo) in
		Basic_Tables.tr 
					(Basic_Tables.td [div [pcdata subject]]) [Basic_Tables.td [div [pcdata duetime]]]
	in  
	Basic_Tables.table 
		(Basic_Tables.tr 
			(Basic_Tables.td [h6 [pcdata "Betreff"]]) [Basic_Tables.td [h6 [pcdata "Datum"]]]
		) 
		(List.map printer active_todos)
		
let todolist_service db = 
	 register_new_service
   	~path:["todos"]
    ~get_params:(suffix (int "year" ** int "month" ** int "day"))
		(fun _ (year,(month,day)) () ->
			try 
				let date = Date.date_of_ints year month day in
				let datestring = Date.string_of_date date in
				let titlestring = Printf.sprintf "Todos für %s" datestring in
				return (
					html 
						(head (title (pcdata titlestring)) [])
					 	(body [
							div [
									h1 [pcdata titlestring];
									show_todos db date
							]
						])
				)
			with Date.Invalid_date ->
				return Error.invalid_date
		)
			