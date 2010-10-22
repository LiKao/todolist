open Date
open Interaction

let choose_repetition ()=
	let repetition_menu =
		"Art der Wiederholung" |$|
		"t" |: "Täglich"        |-> (fun () -> Tododatabase.Daily)                           |%|
		"w" |: "Wöchentlich"    |-> (fun () -> Tododatabase.Weekly (Date.choose_weekday ())) |%|
	(*"m" |: "Monatlich"      |-> (fun () -> Tododatabase.Monthly)                         |%|*)
		"o" |: "Wochentags"     |-> (fun () -> Tododatabase.Weekdays)                        |%|
		"e" |: "Am Wochenende"  |-> (fun () -> Tododatabase.Weekends)
	in
	(Interaction.display_menu repetition_menu) ()
			
let choose_date () =
	{month = January;day=1;year=2010}
	
let add_todo (open_todos,_) () =
	let todo_type_menu =
		"Art des Todoeintrags" |$|
		"h" |: "Heute"              |-> (fun () -> (Tododatabase.Single   (get_today         ()))) |%|
		"e" |: "Einzeltodo"         |-> (fun () -> (Tododatabase.Single   (choose_date       ()))) |%|
		"r" |: "Regelmaeßiges Todo" |-> (fun () -> (Tododatabase.Repeated (choose_repetition ())))
	in
	let todotype = (Interaction.display_menu todo_type_menu) () in
	Printf.printf "Betreff: ";
	flush stdout;
	let todoname = input_line stdin in
	let todo = Tododatabase.make_open todotype todoname in
	Tododatabase.add todo open_todos;
	false
	
let show_todos (open_todos,_) () =
	let printer todo = Printf.printf "%s\n" (Tododatabase.string_of_todo todo) in  
	List.iter printer !open_todos;
	false
	

let close_todo (open_todos,closed_todos) () =
	let todo = Tododatabase.choose open_todos in
	let closed = Tododatabase.close todo (get_today ()) in
	Tododatabase.add closed closed_todos;
	if not (Tododatabase.is_repeated todo) then
		Tododatabase.delete todo open_todos;
	false

let main_menu todos = 
	"Main Menu" |$|
	"h" |: "Todo hinzufuegen"            |-> add_todo   todos  |%|
	"a" |: "aktuelle Todos anzeigen"     |-> show_todos todos  |%|
	"e" |: "Todo als erledigt markieren" |-> close_todo todos  |%|
	"b" |: "Programm beenden"            |-> (fun () -> true)
