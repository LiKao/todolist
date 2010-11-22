open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

let make make_service editservice db = 
	register editservice
		(fun sp () () ->
			let todos = Tododb.get_open db in
			let htmlhead = title (pcdata "Todos bearbeiten") in
			let content =
				[h1 [pcdata "Todos bearbeiten:"]]
				@ (Common.todo_table todos)
			in  
			make_service sp htmlhead content
		)