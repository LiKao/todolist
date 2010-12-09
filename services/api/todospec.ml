open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let make db = 
	Eliom_predefmod.Text.register_new_service
		~path:["spec";"todo"]
		~get_params:unit
		(fun sp () () ->
			Xml.Element ("spec",[],[Todo.t_spec]) |>
			Xml.to_string_fmt |>
			return_xml
	 )