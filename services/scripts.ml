open Common
open XHTML.M
open Eliom_predefmod.Xhtml

let js_script fmt =
  Printf.ksprintf (fun t -> script ~contenttype:"text/javascript" (unsafe_data t)) fmt
	
let options_from_array options string_of_option =
	Array.mapi (
		fun i option ->
			let name = string_of_option options.(i) in
			Option ([],i+1,Some (pcdata name),true)
	) options
	|> Array.to_list
	
let input_field id url target name =
	js_script 
	"
		window.onload=function(){
			var request = new XMLHttpRequest();
			request.open(\"GET\",\"%s\",false);
			request.send();
			
			var element = document.getElementById(\"%s\");
			new formCreator(element,request.responseXML,\"%s\",\"%s\");
		}
	" url id target name
                                                                                      
	