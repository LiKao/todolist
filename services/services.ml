open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

let make_page navigation htmlhead content =
	return (
		html 
		(head htmlhead []) 
		(body
			[div ~a:[a_id "navigation"] (navigation ()); 
		 	 div ~a:[a_id "content"] content
			]
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

let navigation sp () =
	let today = Date.get_today () in
	let tomorrow = Date.next_day today in
	let yesterday = Date.previous_day today in
	[ul ~a:[a_class ["level1"]]
		(li [
			div ~a:[a_class ["li"]] 
				[pcdata "Todos"];
			ul ~a:[a_class ["level2"]]
				(li [
					div ~a:[a_class ["li"]] 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Heute"] (Date.get_year today,(Date.get_monthnum today,Date.get_day today))]
					]
				)
				[li [
					div ~a:[a_class ["li"]] 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Morgen"] (Date.get_year tomorrow,(Date.get_monthnum tomorrow,Date.get_day tomorrow))]
					];
				 li [
					div ~a:[a_class ["li"]] 
						[Eliom_predefmod.Xhtml.a listservice sp [pcdata "Gestern"] (Date.get_year yesterday,(Date.get_monthnum yesterday,Date.get_day yesterday))]
					];
				]
		])
		[li [
			div ~a:[a_class ["li"]]
				[Eliom_predefmod.Xhtml.a editservice sp [pcdata "Todos bearbeiten"] ()]
			]
		]
	]

let make_service sp htmlhead content = 
	make_page (navigation sp)	htmlhead content
	
let register_all db =
	Listservice.make make_service listservice db;
	Editservice.make make_service editservice db
	