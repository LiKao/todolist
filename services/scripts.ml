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
	let month =  (Date.get_month date |> Date.int_of_month) +1 in
	let day   = Date.get_day date in
	div ~a:[a_class ["datechooser"]]
	[
		div ~a:[a_id "calendar"] [];
		Eliom_predefmod.Xhtml.get_form service sp form;
		js_script 
			"
			 var defaultyear = %i;
			 var year_selection = document.getElementById('year');
			 var month_selection = document.getElementById('month');
			 var day_selection = document.getElementById('day');
			
			 function SetDate(year,month,day) {
		
				var days = DaysOfMonth(month,year);
				if (day>days) {
					day=days;
				}
				
				var calendar = document.getElementById('calendar');
				var date = new Date(year,month-1,1);
				function callback(day) {
					SetDate(year,month,day);
				}
				WriteCalendar(calendar,days,date.getDay(),callback);
				
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
				
				var calendarCellId = \"calendarday\" + day;
				var calendarCell = document.getElementById(calendarCellId);
				calendarCell.className = \"selected\";
			 }
			
			 function SetDaysOfMonth() {
			  
				var year = year_selection.value;
				var month = month_selection.value;
				var day = day_selection.value;
				
				SetDate(year,month,day);
			 }
			 
			 function VerifyYear() {
				var year = year_selection.value; 
				var pattern = /^\\d+$/;
				if(!year.match(pattern)) {
					year_selection.value = defaultyear;
				}
				SetDaysOfMonth();
			 }
			
			 window.onload=function(){
				SetDate(defaultyear,%i,%i);
				month_selection.onchange=SetDaysOfMonth;
				year_selection.onchange=VerifyYear;
				day_selection.onchange=SetDaysOfMonth;
			 }
		" year month day
	]
	
let todo_editor todos =
	let ids = 
		List.map Todo.get_id todos |> 
		Array.of_list |> 
		Array.mapi (fun int id -> Printf.sprintf "todos[%i]=%i;\n" int id) |> 
	  Array.fold_left (^) "" in
	js_script
	"
		var todos = new Array();
		%s
		
	  function Make_done(id) {
			var request = new XMLHttpRequest();
			request.open(\"POST\",\"../../../../action/close\");
			request.setRequestHeader(\"Content-type\", \"application/x-www-form-urlencoded\");
			request.send(id);
		}
		
		function Add_button(id) {
			var row = document.getElementById(\"todo\" + id);
			row.innerHTML = row.innerHTML + \"<td><div id=\\\"donebox\" + id + \"\\\"><input type=\\\"checkbox\\\" /></div></td>\";
			
			var donebox = document.getElementById(\"donebox\" + id);
			donebox.onclick = function() {
				Make_done(id);
			}
		}
		
		window.onload=function(){
			for(i=0;i<todos.length;i++){
				Add_button(todos[i]);
			}
		}
	" ids
	