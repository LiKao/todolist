TYPE_CONV_PATH "Date"

(** Years **)

type year = int with sexp

let is_leapyear year =
	(year mod 1000) = 0 || ((year mod 4) = 0 && (year mod 100) !=0) 


(** Months **)

type month = January
           | February
					 | March
				   | April
					 | May
					 | June
					 | July
					 | August
					 | September
					 | Oktober
					 | November
					 | December 
					 with sexp 
					
let int_of_month = 
	function
		January    ->      0
	|	February   ->      1 
	| March      ->      2
	|	April      ->      3
	|	May        ->      4
	|	June       ->      5
	|	July       ->      6
	|	August     ->      7
	|	September  ->      8
	|	Oktober    ->      9
	|	November   ->     10
	|	December   ->     11	
	
let string_of_month = 
	function
	  January -> "Januar"
	|	February -> "Februar"
	| March -> "März"
	|	April -> "April"
	|	May -> "Mai"
	|	June -> "Juni"
	|	July -> "Juli"
	|	August -> "August"
	|	September -> "September"
	|	Oktober -> "Oktober"
	|	November -> "November"
	|	December -> "Dezember"

let months = [|
			January;
      February;
		  March;
			April;
		  May;
			June;
			July;
			August;
			September;
			Oktober;
			November;
			December|]
			
let choose_month =
	let month_menu = Interaction.choices_of_array months string_of_month "Monat auswählen" in
	(fun () -> Interaction.display_choice month_menu)
	
let compare_month month1 month2 =
	let monthnum1 = int_of_month month1 in
	let monthnum2 = int_of_month month2 in
	monthnum1-monthnum2
			

(** Days in a month **)
			
type dayofmonth = int with sexp					
						
(** Weekdays **)						
																		
type weekday =
	| Sunday 
	| Monday
  | Tuesday
	| Wednesday
	| Thursday
	| Friday
	| Saturday
  with sexp

let string_of_weekday = 
	function
  	Sunday    -> "Sonntag"
	|	Monday    -> "Montag"
	|	Tuesday   -> "Dienstag"
	|	Wednesday -> "Mittwoch"
	|	Thursday  -> "Donnerstag"
	|	Friday    -> "Freitag"
	|	Saturday  -> "Samstag"


let int_of_weekday =
	function
	|	Sunday    -> 0
	|	Monday    -> 1
	|	Tuesday   -> 2
	|	Wednesday -> 3
	|	Thursday  -> 4
	|	Friday    -> 5
	|	Saturday  -> 6
	 
	
let weekdays = [|
   Sunday;
	 Monday;
	 Tuesday;
	 Wednesday;
	 Thursday;
	 Friday;
	 Saturday|]
	
let choose_weekday =
	let weekday_menu = Interaction.choices_of_array weekdays string_of_weekday "Wochentag auswählen" in
	(fun () -> Interaction.display_choice weekday_menu)
	
(** complete dates (year,month and day) **)
	
type date = {month : month;
             day : dayofmonth;
						 year : year}
						with sexp
						
let compare date1 date2 =
	if date1.year != date2.year then
		date1.year-date2.year
	else
		let monthdiff = compare_month date1.month date2.month in
		if monthdiff != 0 then
			monthdiff
		else
			date1.day-date2.day
			
let string_of_date date =
	let weekday = get_weekday date in
	let weekdaystring = string_of_weekday weekday in
	let month   = string_of_month date.month in 
	Printf.sprintf " %s %i. %s %i" weekdaystring date.day month date.year	
	
(** Accesor functions **)

let get_today () =
	let u_time = Unix.localtime (Unix.time ()) in
	{month = months.(u_time.Unix.tm_mon);
	 day = u_time.Unix.tm_mday;
	 year = u_time.Unix.tm_year + 1900}
	
let is_today date = 
	compare (get_today ()) date = 0	  
	
let get_weekday date =
	let century = date.year / 100 in
	let century_item = 2 * (3 - (century mod 4)) in
	let year_number = date.year - century * 100 in
	let year_item = year_number + year_number/4 in
	let month_table = [
			(January, if is_leapyear date.year then 6 else 0);
			(February,if is_leapyear date.year then 2 else 3);
		  (March,    3);
			(April,    6);
			(May,      1);
			(June,     4);
			(July,     6);
			(August,   2);
			(September,5);
			(Oktober,  0);
			(November, 3);
			(December, 5)]
	in
	let month_item = List.assoc date.month month_table in
	let day_number = (century_item + year_item + month_item + date.day) mod 7 in
	weekdays.(day_number)
	
	
