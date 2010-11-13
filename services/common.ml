open XHTML.M
open Eliom_predefmod.Xhtml

let todo_table todos =
	let printer todo =  
		let subject = Todo.get_subject todo in
		let duetime = Todo.string_of_todotime (Todo.get_duetime todo) in
		Basic_Tables.tr 
					(Basic_Tables.td [div ~a:[a_class ["todosubject"]] [pcdata subject]]) 
					 [Basic_Tables.td ~a:[a_class ["todotime"]] [div [pcdata duetime]]]
	in  
	Basic_Tables.table 
		(Basic_Tables.tr 
			(Basic_Tables.td [div ~a:[a_class ["tableheader"]] [pcdata "Betreff"]]) 
			[Basic_Tables.td [div ~a:[a_class ["tableheader"]] [pcdata "Datum"]]]
		) 
		(List.map printer todos) 