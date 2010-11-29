open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml


let make_doneaction make_service =
  Eliom_predefmod.Action.register_new_service
    ~get_params:(Eliom_parameters.int "id")
		~path:["actions";"close"]
    (fun sp id () -> Printf.printf "Schließe id %i\n" id; flush stdout; return ())
