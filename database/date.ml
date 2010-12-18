open Helpers
open BatStd

(** Exceptions **)

exception Invalid_date

(** Years **)

type year = int

let is_leapyear year =
	(year mod 1000) = 0 || ((year mod 4) = 0 && (year mod 100) !=0) 
	
let xml_of_year year =
	Xml.Element ("year",[],[Xml.PCData (string_of_int year)])
	
let year_of_xml xml =
	Printf.printf "year_of_xml\n"; flush stdout;
	Xmlhelpers.check_node 
		xml
		~name:"year"
		~node_error:"year_of_xml: Not an element node"
		~type_error:"year_of_xml: Not a year node";
	xml |>
	Xmlhelpers.access_text |>
	int_of_string

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

let month_of_int i =
	months.(i)
	
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


let days_of_month month year =
	match month with
		January -> 31
	|	February -> if is_leapyear year then 29 else 28
	| March -> 31
	|	April -> 30
	|	May -> 31
	|	June -> 30
	|	July -> 31
	|	August -> 31
	|	September -> 30
	|	Oktober -> 31
	|	November -> 30
	|	December -> 31 	

		
let next_month month = 
	let monthnum = int_of_month month in
	months.((monthnum + 1) mod 12)
	
let previous_month month =
	let monthnum = int_of_month month in
	months.((monthnum + 11) mod 12)
			
let compare_month month1 month2 =
	let monthnum1 = int_of_month month1 in
	let monthnum2 = int_of_month month2 in
	monthnum1-monthnum2
	
let xml_of_month month =
	Xml.Element ("month",[],
		(Xmlhelpers.xml_of_named 
			month 
			"number" 
			(int_of_month |- string_of_int) 
			string_of_month))
			
let month_of_xml xml =
	Printf.printf "month_of_xml\n"; flush stdout;
	Xmlhelpers.check_node
		xml
		~name:"month"
		~node_error:"month_of_xml: not a element node"
		~type_error:"month_of_xml: not a month node";
	xml |>
	Xmlhelpers.find_child "number" |>
	Xmlhelpers.access_text |>
	int_of_string |>
	month_of_int
	
(** Days in a month **)
			
type dayofmonth = int

let dayofmonth_spec =
	Xml.Element ("intrange",[
		("fieldname","day");
		("min","1");
		("max","31")
	],[])

let xml_of_day day =
	Xml.Element ("day",[],[Xml.PCData (string_of_int day)])
	
let day_of_xml xml =
	Printf.printf "day_of_xml\n"; flush stdout;
	Xmlhelpers.check_node
		xml
		~name:"day"
		~node_error:"day_of_xml: Not an element node"
		~type_error:"day_of_xml: Not a day node";
	xml |>
	Xmlhelpers.access_text |>
	int_of_string
						
(** Weekdays **)						
																		
type weekday =
	| Sunday 
	| Monday
  | Tuesday
	| Wednesday
	| Thursday
	| Friday
	| Saturday

let weekdays = [|
   Sunday;
	 Monday;
	 Tuesday;
	 Wednesday;
	 Thursday;
	 Friday;
	 Saturday|]

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

let weekday_of_int i =
	weekdays.(i)

let weekday_spec =
	let make_entry weekday =
		Xml.Element ("variant_entry",[
				("name",string_of_weekday weekday);
				("fieldname","number")
			],[
				Xml.Element ("value",[
					],[
						Xml.PCData (weekday |> int_of_weekday |> string_of_int)
				]) 
		])
	in
	Xml.Element ("variant",[("fieldname","weekday")],[
		make_entry Sunday;
		make_entry Monday;
		make_entry Tuesday;
		make_entry Wednesday;
		make_entry Thursday;
		make_entry Friday;
		make_entry Saturday
	])
		
let is_weekday =
	function
	|	Sunday    -> false
	|	Monday    -> true
	|	Tuesday   -> true
	|	Wednesday -> true
	|	Thursday  -> true
	|	Friday    -> true
	|	Saturday  -> false

let is_weekend weekday =
	not (is_weekday weekday)
	
let xml_of_weekday weekday =
	Xml.Element ("weekday",[],
		(Xmlhelpers.xml_of_named 
			weekday 
			"number"
			(int_of_weekday |- string_of_int)
			string_of_weekday))
			
let weekday_of_xml xml =
	Xmlhelpers.check_node 
		xml
		~name:"weekday"
		~node_error:"Date.weekday_of_xml: not a element node"
		~type_error:"Date.weekday_of_xml: not a weekday node";
	xml |>
	Xmlhelpers.find_child "number" |>
	Xmlhelpers.access_text |>
	int_of_string |>
	weekday_of_int
	
(** complete dates (year,month and day) **)
	
type date = {month : month;
             day : dayofmonth;
			 year : year}
			
let date_spec = Xml.Element ("date",[],[])
						
let date_of_ints year monthnum day =
	if monthnum > 12 or monthnum < 1 then
		raise Invalid_date;
	let month = months.(monthnum - 1) in
	if day > days_of_month month year then
		raise Invalid_date; 
	{year = year;
	 day = day;
	 month = month}
						
let compare date1 date2 =
	if date1.year != date2.year then
		date1.year-date2.year
	else
		let monthdiff = compare_month date1.month date2.month in
		if monthdiff != 0 then
			monthdiff
		else
			date1.day-date2.day
			
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
			
let string_of_date date =
	let weekday = get_weekday date in
	let weekdaystring = string_of_weekday weekday in
	let month   = string_of_month date.month in 
	Printf.sprintf " %s %i. %s %i" weekdaystring date.day month date.year
	
let xml_of_date date =
	Xml.Element ("date",[],[
		(xml_of_day date.day);
		(xml_of_month date.month);
		(xml_of_year date.year);
		(xml_of_weekday (get_weekday date));])
		
let date_of_xml xml =
	Xmlhelpers.check_node 
		xml
		~name:"date"
		~node_error:"date_of_xml: not an element node"
		~type_error:"date_of_xml: not a date node";
	let day =
		xml |>
		Xmlhelpers.find_child "day" |>
		day_of_xml
	in
	let month =
		xml |>
		Xmlhelpers.find_child "month" |>
		month_of_xml
	in
	let year =
		xml |>
		Xmlhelpers.find_child "year" |>
		year_of_xml
	in
	{year = year;
	 month = month;
	 day = day}
	
let get_month date =
	date.month
	
let get_monthnum date =
	(int_of_month date.month) + 1
	
let get_day date =
	date.day
	
let get_year date =
	date.year
	
(** Accesor functions **)

let get_today () =
	let u_time = Unix.localtime (Unix.time ()) in
	{month = months.(u_time.Unix.tm_mon);
	 day = u_time.Unix.tm_mday;
	 year = u_time.Unix.tm_year + 1900}
	
let is_today date = 
	compare (get_today ()) date = 0	  
	
(** Calculations and Queries **)

let is_last_day_of_month date =
	date.day = (days_of_month date.month date.year)
	
let is_first_day_of_month date =
	date.day = 1
	
let is_last_day_of_year date =
	date.day = 31 && date.month = December
	
let is_first_day_of_year date =
	date.day = 1 && date.month = January
	
let next_day date =
	if is_last_day_of_year date then
		{day = 1; month = January; year = date.year +1}
	else if is_last_day_of_month date then
		{day = 1; month = next_month date.month; year = date.year}
	else
		{date with day = date.day +1}
		
let previous_day date =
	if is_first_day_of_year date then
		{day = 31; month = December; year = date.year -1}
	else if is_first_day_of_month date then
		let prev_month = previous_month date.month in
		let day = days_of_month prev_month date.year in
		{day = day; month = prev_month; year = date.year}
	else
		{date with day = date.day -1}
		
let rec increment date times =
	if times <= 0 then
		date
	else
		increment (next_day date) (times-1)

let rec decrement date times =
	if times <= 0 then
		date
	else
		decrement (previous_day date) (times-1)
		
let rec next_weekdate date weekday =
	if get_weekday date = weekday then
		date
	else
		next_weekdate (next_day date) weekday
		
let rec previous_weekdate date weekday =
	(* if the current date matches the weekday, we want the one before that *)
	let date = previous_day date in
	if get_weekday date = weekday then
		date
	else
		previous_weekdate date weekday
		
let next_monthdate date dayofmonth =
	let res =
		if date.day > dayofmonth then
			if date.month = December then
				{month = January;
			 	 day = dayofmonth;
				 year = date.year + 1}
			else
				{month = next_month date.month;
				 day = dayofmonth;
				 year = date.year}
		else
			{date with day = dayofmonth}
	in
	let max_days = days_of_month res.month res.year in
	if res.day > max_days then
		{res with day = max_days}
	else
		res
		
let previous_monthdate date dayofmonth =
	let res =
		if date.day < dayofmonth then
			if date.month = January then
				{month = December;
			 	 day = dayofmonth;
				 year = date.year - 1}
			else
				{month = previous_month date.month;
				 day = dayofmonth;
				 year = date.year}
		else
			{date with day = dayofmonth}
	in
	let max_days = days_of_month res.month res.year in
	if res.day > max_days then
		{res with day = max_days}
	else
		res 

let julian_day date =
	let monthnum = (int_of_month date.month)+1 in
	let y,m = 
		if monthnum > 2 then 
			float_of_int |< (date.year,monthnum)
		else
			float_of_int |< (date.year-1,monthnum+12)
	in
	let yeardays = int_of_float ((y+.4716.0)*.365.25) in
	let monthdays = int_of_float (m*.30.6001) in
	yeardays + monthdays + date.day - 33
			 