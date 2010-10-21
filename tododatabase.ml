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
					 | December with sexp 

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

type date = {month : month;
             day : int}
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

type repetition = Dayly
                | Weekly of weekday
								| Monthly of int
								| Weekdays
								| Weekends
								with sexp

type todotime = Repeated of repetition
              | Single   of date
							with sexp

type tododata = {duetime : todotime;
                 subject : string}
								with sexp

(* States of todos as phantom types to allow generic type*)
(* checked data bases *)																
type pending
type closed
						
(* The actual type of todos 'a can be pending or closed *)																								
type 'a todo = tododata with sexp 

              
         