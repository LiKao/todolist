open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

  

type navitem_t = 
  {title: string;
   target: uri option;
   subs: navbar_t}

and navbar_t = 
  navitem_t list

let make_navitem title ?target ?(subs=[]) ()=
  {title=title;
   subs=subs;
   target=target}
 
let make_navigation id target_id navbar =
  let rec make_listitem i navitem =
		li ([
	 		linkordiv_with_class "navitem" navitem.target
			[
  	 		pcdata navitem.title
    	] 
    ]	@ (enter (i+1) navitem.subs))
  and enter i =
      function
        navitem :: navitems -> 
          [ul ~a:[a_class [Printf.sprintf "level%i" i]]
             (make_listitem i navitem)
             (loop i navitems)
          ]
      | [] -> []
  and loop i =
    function
   	  navitem :: navitems ->
        (make_listitem i navitem) :: (loop i navitems)
    | [] -> []
  in
	[div_with_id id (enter 1 navbar)]
     
    
    
  
