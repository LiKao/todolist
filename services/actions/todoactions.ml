open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml


let make_doneaction make_service =
	let fallback =
		register_new_service
    ~path:["action";"close"]
    ~get_params:unit
		(fun sp () () ->
			Error.post_error make_service sp
		)
	in
  Eliom_predefmod.Action.register_new_post_service
    ~post_params:(Eliom_parameters.int "state")
    (fun sp () id -> Printf.printf "Schließe id %i\n" id; flush stdout; return ())
		~fallback