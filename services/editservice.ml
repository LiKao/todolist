open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

let make make_service editservice db = 
	register editservice
		(fun sp () () ->
		 (Tododb.get_open db) >>= fun todos ->
		 let htmlhead = title (pcdata "Todos bearbeiten") in
		 return (
				[h1 [pcdata "Todos bearbeiten:"]]
				@ (Common.todo_table todos))
     	 >>= fun content ->
		 make_service sp htmlhead content
		)