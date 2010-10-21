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
					
type dayofmonth = int with sexp	

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
	
type date = {month : month;
             day : dayofmonth}
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
	
let weekdays = [|
   Sunday;
	 Monday;
	 Tuesday;
	 Wednesday;
	 Thursday;
	 Friday;
	 Saturday|]