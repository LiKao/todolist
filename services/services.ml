open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
		
let create db = 
	ignore (Todoservice.todolist_service db)