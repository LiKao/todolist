open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let show_todos db date =
	let active_todos = Tododb.get_active db date in
	let script = Scripts.todo_editor active_todos in
	(Common.todo_table active_todos) @ [script]
		
let make_daylist make_service listservice db = 
	register listservice
		(fun sp (year,(month,day)) () ->
			try 
				let date = Date.date_of_ints year month day in
				let datestring = Date.string_of_date date in
				let titlestring = Printf.sprintf "Todos für %s" datestring in
				let htmlhead = title (pcdata titlestring) in
				let content =
					[h1 [pcdata titlestring]]
					@ (show_todos db date)
				in  
				make_service sp htmlhead content
			with Date.Invalid_date ->
				Error.invalid_date make_service sp
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
	
			