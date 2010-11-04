open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

include Service_base

let make_page path head body =
	register_new_service ~path:[path] ~get_params:unit
	(fun _ () () ->  return (html head body)) 
			   					
					
let create db = 
	let f () = 
		div 
		[
			h1 [pcdata "Todos für heute"];
	  	Todoservice.show_todos db	(Date.get_today ())
		]
	in
	let head = make_head "Hallo" in
	let body = make_inner_body f in
	ignore (make_page "" head body);
	ignore (Todoservice.todolist_service db)