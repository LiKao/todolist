open XHTML.M
open Eliom_predefmod.Xhtml

let (|>) a f = f a

let div_with_class klass ?(a = []) l = div ~a:(a_class [klass] :: a) l
let div_with_id id ?(a = []) l = div ~a:(a_id id :: a) l
let js_script_ext ~src =
  script ~contenttype:"text/javascript" ~a:[a_src (uri_of_string src)] (pcdata "")

let todo_table todos =
	let printer todo =  
		let subject = Todo.get_subject todo in
		let duetime = Todo.string_of_todotime (Todo.get_duetime todo) in
		Basic_Tables.tr 
					(Basic_Tables.td [div_with_class "todosubject" [pcdata subject]]) 
					[Basic_Tables.td [div_with_class "todotime"    [pcdata duetime]]]
	in  
	Basic_Tables.table 
		(Basic_Tables.tr 
			(Basic_Tables.td [div_with_class "tableheader" [pcdata "Betreff"]]) 
			[Basic_Tables.td [div_with_class "tableheader" [pcdata "Datum"]]]
		) 
		(List.map printer todos) 