open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Common

let scripts sp =
	[js_script ~uri:(make_uri ~service:(static_dir sp) ~sp ["scripts";"date.js"]) ()]

let make_page sp navigation htmlhead content =
	return (
		html 
		(head htmlhead [css_link ~uri:(make_uri ~service:(static_dir sp) ~sp ["styles";"style.css"]) ()]) 
		(body
			[div_with_id "scripts" (scripts sp);
			 div_with_id "navigation" (navigation sp); 
		 	 div_with_id "content" content
			]
		)
	)		
						
let listservice = 
	Eliom_services.new_service 
		~path:["todos"] 
		~get_params:(suffix (int "year" ** int "month" ** int "day")) 
		()

let chooserservice = 
		Eliom_services.new_service 
		~path:["choose"] 
		~get_params:unit 
		()			
						
let editservice =
	Eliom_services.new_service
	~path:["edit"]
	~get_params:unit
	()
	


let navigation sp =
	let today = Date.get_today () in
	let tomorrow = Date.next_day today in
	let yesterday = Date.previous_day today in
	[ul ~a:[a_class ["level1"]]
		(li [
			div_with_class "li" 
				[Eliom_predefmod.Xhtml.a chooserservice sp [pcdata "Todos"] ()];
			ul ~a:[a_class ["level2"]]
				(li [
					div_with_class "li" 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Heute"] (Date.get_year today,(Date.get_monthnum today,Date.get_day today))]
					]
				)
				[li [
					div_with_class "li" 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Morgen"] (Date.get_year tomorrow,(Date.get_monthnum tomorrow,Date.get_day tomorrow))]
					];
				 li [
					div_with_class "li" 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Gestern"] (Date.get_year yesterday,(Date.get_monthnum yesterday,Date.get_day yesterday))]
					];
				]
		])
		[li [
			div_with_class "li"
				[Eliom_predefmod.Xhtml.a editservice sp [pcdata "Todos bearbeiten"] ()]
			]
		]
	]

let make_service sp htmlhead content = 
	make_page sp navigation	htmlhead content
	
let register_all db =
	Listservice.make_daylist make_service listservice db;
	Listservice.make_todochooser make_service listservice chooserservice;
	Editservice.make make_service editservice db
	