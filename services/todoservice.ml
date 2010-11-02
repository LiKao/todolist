open XHTML.M
open Eliom_predefmod.Xhtml

let show_todos db =
	let active_todos = Tododb.get_active db (Date.get_today ()) in
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