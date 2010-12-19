open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let scripts sp =
	[js_script ~uri:(make_uri ~service:(static_dir sp) ~sp ["scripts";"date.js"]) ();
	 js_script ~uri:(make_uri ~service:(static_dir sp) ~sp ["scripts";"navi.js"]) ();
	 js_script ~uri:(make_uri ~service:(static_dir sp) ~sp ["scripts";"input.js"]) ()
	]

let make_page sp navigation htmlhead content =
	let content_name = "content" in
	return (
		html 
		(head htmlhead [css_link ~uri:(make_uri ~service:(static_dir sp) ~sp ["styles";"style.css"]) ()]) 
		(body (
			[div_with_id "scripts" (scripts sp)] @
			(navigation content_name sp) @ 
		 	[div_with_id content_name content]
		)
		)
	)		
						
let listservice = 
	Eliom_services.new_service 
		~path:["todos"] 
		~get_params:(suffix (int "year" ** int "month" ** int "day")) 
		()

let editservice =
	Eliom_services.new_service
	~path:["edit"]
	~get_params:unit
	()
	
let navigation target_id sp =
	let today = Date.get_today () in
	let todayservice = 
		Eliom_services.preapply listservice 
		  (Date.get_year today,(Date.get_monthnum today,Date.get_day today)) in
	let tomorrow = Date.next_day today in
	let tomorrowservice = 
		Eliom_services.preapply listservice 
		  (Date.get_year tomorrow,(Date.get_monthnum tomorrow,Date.get_day tomorrow)) in 
	let yesterday = Date.previous_day today in
	let yesterdayservice = 
		Eliom_services.preapply listservice 
			(Date.get_year yesterday,(Date.get_monthnum yesterday,Date.get_day yesterday)) in
	let navbar = 
   [Navigation.make_navitem "Todos" ~subs:[
			Navigation.make_navitem "Heute"   ~target:(js_target ~funct:"ReLoad" ~service:todayservice     ~sp) ();
			Navigation.make_navitem "Morgen"  ~target:(js_target ~funct:"ReLoad" ~service:tomorrowservice  ~sp) ();
			Navigation.make_navitem "Gestern" ~target:(js_target ~funct:"ReLoad" ~service:yesterdayservice ~sp) ();
		] ();
    Navigation.make_navitem "Todos bearbeiten" ~target:(make_uri ~service:editservice ~sp ()) ();
	]
	in
	Navigation.make_navigation "navigation" target_id navbar

let make_service sp htmlhead content = 
	make_page sp navigation	htmlhead content
	
let register_all db =
	Listservice.make_daylist listservice db;
	Editservice.make make_service editservice db
	
let done_action = Todoactions.make_doneaction make_service
	
	
let at_exit command =	
	let hook () = return () >|= command in
	Lwt_main.at_exit hook
	
	