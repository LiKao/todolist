TYPE_CONV_PATH "Current.Module.Name"

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
					
let monthstrings = [
	(January,"Januar");
	(February,"Februar");
  (March,"März");
	(April,"April");
	(May,"Mai");
	(June,"Juni");
	(July,"Juli");
	(August,"August");
	(September,"September");
	(Oktober,"Oktober");
	(November,"November");
	(December,"Dezember")]
	
let choose_month =
	let month_menu = Interaction.make_choices monthstrings "Monat auswählen" in
	(fun () -> Interaction.display_choice month_menu)
		
	
let monthnumbers = [
	(January,   0 );
	(February,  1 );
  (March,     2 );
	(April,     3 );
	(May,       4 );
	(June,      5 );
	(July,      6 );
	(August,    7 );
	(September, 8 );
	(Oktober,   9 );
	(November,  10);
	(December,  11)]	
	
let string_of_month month = List.assoc month monthstrings
	
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
			
type dayofmonth = int with sexp	
type year = int with sexp			
	
type date = {month : month;
             day : dayofmonth;
						 year : year}
						with sexp
						
type weekday = Monday
             | Tuesday
						 | Wednesday
						 | Thursday
						 | Friday
						 | Saturday
						 | Sunday
						 with sexp

let weekstrings = [
	(Monday,"Montag");
	(Tuesday,"Dienstag");
	(Wednesday,"Mittwoch");
	(Thursday,"Donnerstag");
	(Friday,"Freitag");
	(Saturday,"Samstag");
	(Sunday,"Sonntag")]
	
let choose_weekday =
	let weekday_menu = Interaction.make_choices weekstrings "Wochentag auswählen" in
	(fun () -> Interaction.display_choice weekday_menu)
	
let string_of_weekday weekday = List.assoc weekday weekstrings 
	
let weekdays = [|
   Sunday;
	 Monday;
	 Tuesday;
	 Wednesday;
	 Thursday;
	 Friday;
	 Saturday|]
	
let compare_month month1 month2 =
	let monthnum1 = List.assoc month1 monthnumbers in
	let monthnum2 = List.assoc month2 monthnumbers in
	monthnum1-monthnum2
	
let compare date1 date2 =
	if date1.year != date2.year then
		date1.year-date2.year
	else
		let monthdiff = compare_month date1.month date2.month in
		if monthdiff != 0 then
			monthdiff
		else
			date1.day-date2.day
	   
let get_today () =
	let u_time = Unix.localtime (Unix.time ()) in
	{month = months.(u_time.Unix.tm_mon);
	 day = u_time.Unix.tm_mday;
	 year = u_time.Unix.tm_year + 1900}
	
let is_today date = 
	compare (get_today ()) date = 0
  
	
let is_leapyear year =
	(year mod 1000) = 0 || ((year mod 4) = 0 && (year mod 100) !=0) 
	
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
	
	
