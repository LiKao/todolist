open XHTML.M
open Eliom_predefmod.Xhtml

let (|>) a f = 
	f a

let (@>|) (ls,item) f =
	ls @ (f item)

let div_with_class klass ?(a = []) l = div ~a:(a_class [klass] :: a) l
let div_with_id id ?(a = []) l = div ~a:(a_id id :: a) l
let linkordiv_with_class klass target ?(a = []) =
	match target with
	  Some target -> XHTML.M.a ~a:([a_href target; a_class [klass]] @ a)
	| None -> div ~a:([a_class [klass]])


let js_target ~funct ~sp ~service =
	Printf.sprintf "javascript:%s('%s')" funct (make_string_uri ~service ~sp ()) |>
	uri_of_string
	
	  
let js_script_ext ~src =
  script ~contenttype:"text/javascript" ~a:[a_src (uri_of_string src)] (pcdata "")

let todo_table todos =
	let printer todo =  
		let subject = Todo.get_subject todo in
		let duetime = Todo.string_of_todotime (Todo.get_duetime todo) in
		Basic_Tables.tr ~a:[a_class ["todo"]]
					(Basic_Tables.td [div_with_class "todosubject" [pcdata subject]]) 
					[Basic_Tables.td [div_with_class "todotime"    [pcdata duetime]]]
	in  
	[Basic_Tables.table 
		(Basic_Tables.tr 
			(Basic_Tables.td [div_with_class "tableheader" [pcdata "Betreff"]]) 
			[Basic_Tables.td [div_with_class "tableheader" [pcdata "Datum"]]]
		) 
		(List.map printer todos)]
		
let return_xml xml =
	Lwt.return (xml,"text/xml")