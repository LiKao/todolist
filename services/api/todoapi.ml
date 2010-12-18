open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let make_data db = 
	Eliom_predefmod.Text.register_new_service
		~path:["data";"todos"]
		~get_params:(suffix (int "year" ** int "month" ** int "day"))
		(fun sp (year,(month,day)) () ->
			let date = Date.date_of_ints year month day in
			Tododb.get_active db date >>= fun active_todos ->
			Tododb.xml_of_todolist active_todos |>
			Xml.to_string_fmt |>
			return_xml
	 )
	
let make_spec db = 
	Eliom_predefmod.Text.register_new_service
		~path:["spec";"todo"]
		~get_params:unit
		(fun sp () () ->
			Xml.Element ("spec",[],[Todo.t_spec]) |>
			Xml.to_string_fmt |>
			return_xml
	 )
				 