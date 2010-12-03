open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let show_todos db date =
	Tododb.get_active db date >>= fun active_todos ->
	let script = Scripts.todo_editor active_todos in
	return ((Common.todo_table active_todos) @ [script])
		
let make_daylist listservice db = 
	Eliom_predefmod.Blocks.register listservice
		(fun sp (year,(month,day)) () ->
         	try 
				let date = Date.date_of_ints year month day in
				let datestring = Date.string_of_date date in
				let titlestring = Printf.sprintf "Todos für %s" datestring in
				let htmlhead = title (pcdata titlestring) in
            	show_todos db date >>= fun todotable ->
				return ([h1 [pcdata titlestring]] @ (todotable)) 
				>>= fun content ->
				return content
			with Date.Invalid_date ->
				return [h1 [pcdata "error"]]
		)
		
let make_todochooser make_service listservice chooserservice =
	register chooserservice
		(fun sp () () ->
			let htmlhead = title (pcdata "Bitte Tag auswählen") in
			let content =
				[
					h1 [pcdata "Bitte Tag auswählen"];
					Scripts.choose_date listservice sp
				]
			in
			make_service sp htmlhead content
		)
	
			