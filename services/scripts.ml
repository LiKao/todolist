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
		let monthoptions = options_from_array Date.months Date.string_of_month in
		 [p [Eliom_predefmod.Xhtml.int_select ~a:[a_id "day"]   (Option ([a_disabled `Disabled],0,Some (pcdata "Tag"),false)) [] ~name:day;
         Eliom_predefmod.Xhtml.int_select ~a:[a_id "month"] (Option ([a_disabled `Disabled],0,Some (pcdata "Monat"),false)) monthoptions ~name:month;
         Eliom_predefmod.Xhtml.int_input ~a:[a_id "year"] ~input_type:`Text ~name:year ();
         Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Click" ()]]
	in
	let date  = Date.get_today () in
	let year  = Date.get_year date in
	let month =  Date.get_month date |> Date.int_of_month in
	let day   = Date.get_day date in
	div ~a:[a_class ["datechooser"]]
	[
		Eliom_predefmod.Xhtml.get_form service sp form;
		js_script 
			"
			 function SetDate(year,month,day) {
			  var year_selection  = document.getElementById('year');
			  var month_selection = document.getElementById('month');
				var day_selection   = document.getElementById('day');
				
				var days = DaysOfMonth(month,year);
				if (day>days) {
					day=days;
				}
				
				day_selection.options.length=0;
				
				var title = new Option(\"Tag\");
				title.disabled=true;
				try {
				  day_selection.add(title,null);
				} catch(e) {
					day_selection.add(title);
			  }
				
				for (var i = 1; i <= days; i++) {
					var opt = new Option(i);
					try {
						day_selection.add(opt,null);
					} catch(e) {
						day_selection.add(opt);
					}
				}
				year_selection.value = year;
				month_selection.value = month;
				day_selection.value = day;
			 }
			
			 function SetDaysOfMonth() {
			  var year_selection = document.getElementById('year');
				var year = year_selection.value;
				var month_selection = document.getElementById('month');
				var month = month_selection.value;
				var day_selection = document.getElementById('day');
				var day = day_selection.value;
				
				SetDate(year,month,day);
			 }
			 
			
			 window.onload=function(){
				SetDate(%i,%i,%i);
				document.getElementById('month').onchange=SetDaysOfMonth;
				document.getElementById('year').onchange=SetDaysOfMonth;
			 }
		" year month day
	]
	
	
	