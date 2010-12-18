open BatStd

exception Parse_Error of string

let xml_of_named value teipe val_to_string val_to_name =
	[Xml.Element (teipe,[], [Xml.PCData (val_to_string value)]);
	 Xml.Element ("name",[],[Xml.PCData (val_to_name   value)])]
	
let find_child name node =
	Xml.children node |>
	List.find 
		(fun child ->
			try (Xml.tag child) = name
			with Xml.Not_element _ -> false)
			
let get_single_child node =
	Xml.children node |>
	List.hd
			
let access_text node =
	try
	  Xml.children node |>
	  List.hd |>
	  Xml.pcdata
 with 
	| Failure "hd"     -> raise (Parse_Error "No pcdata")
  | Xml.Not_pcdata _ -> raise (Parse_Error "No text")

let is_element node =
	try 
		begin 
			ignore (Xml.tag node);
			true
		end
	with Xml.Not_element _ -> false 
	
let check_node node ~name ~node_error ~type_error =
	if not (is_element node) then
		raise (Parse_Error node_error);   
	if not ((Xml.tag node) = name) then
		raise (Parse_Error type_error)
		
let produce values node =
	let name = Xml.tag node in
	let producer =
		try	List.assoc name values
		with Not_found -> raise (Parse_Error "Xmlhelpers.produce") 
	in
	producer node
		