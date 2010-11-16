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
	
let choose_date service sp =
	let form (year,(month,day)) =
		let date = Date.get_today () in
		let yearval = Date.get_year date in
		let monthoptions = options_from_array Date.months Date.string_of_month in
		 [p [Eliom_predefmod.Xhtml.int_select ~a:[a_id "day"]   (Option ([a_disabled `Disabled],0,Some (pcdata "Tag"),false)) [] ~name:day;
         Eliom_predefmod.Xhtml.int_select ~a:[a_id "month"] (Option ([a_disabled `Disabled],0,Some (pcdata "Monat"),false)) monthoptions ~name:month;
         Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:year ~value:yearval ();
         Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Click" ()]]
	in
	div ~a:[a_class ["datechooser"]]
	[
		Eliom_predefmod.Xhtml.get_form service sp form;
		js_script 
			"
			 function SetDaysOfMonth() {
				var month = this.value;
				var day_selection = document.getElementById('day');
				var days = DaysOfMonth(month);
				day_selection.options.length=0;
				
				for (var i = 1; i <= days; i++) {
					var opt = new Option(i);
					try {
						day_selection.add(opt,null);
					} catch(e) {
						day_selection.add(opt);
					}
				}
			 }
			 
			 window.onload=function(){
				document.getElementById('month').onchange=SetDaysOfMonth;
			 }
		"
	]
	
	
	