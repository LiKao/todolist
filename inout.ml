open Date
open Interaction

let choose_repetition ()=
	let repetition_menu =
		"Art der Wiederholung" |$|
		"t" |: "Täglich"        |-> (fun () -> Todo.Daily)                           |%|
		"w" |: "Wöchentlich"    |-> (fun () -> Todo.Weekly (Date.choose_weekday ())) |%|
	(*"m" |: "Monatlich"      |-> (fun () -> Todo.Monthly)                         |%|*)
		"o" |: "Wochentags"     |-> (fun () -> Todo.Weekdays)                        |%|
		"e" |: "Am Wochenende"  |-> (fun () -> Todo.Weekends)
	in
	(Interaction.display_menu repetition_menu) ()
			
let choose_date () =
	{month = January;day=1;year=2010}
	
let add_todo db () =
	let todo_type_menu =
		"Art des Todoeintrags" |$|
		"h" |: "Heute"              |-> (fun () -> (Todo.Single   (get_today         ()))) |%|
		"e" |: "Einzeltodo"         |-> (fun () -> (Todo.Single   (choose_date       ()))) |%|
		"r" |: "Regelmaeßiges Todo" |-> (fun () -> (Todo.Repeated (choose_repetition ())))
	in
	let todotype = (Interaction.display_menu todo_type_menu) () in
	Printf.printf "Betreff: ";
	flush stdout;
	let todoname = input_line stdin in
	let todo = Todo.make_open todotype todoname in
	Tododatabase.add todo db;
	false
	
let show_todos db () =
	let printer todo = Printf.printf "%s\n" (Todo.string_of_todo todo) in  
	List.iter printer db.Tododatabase.open_todos;
	false
	

let close_todo db () =
	let todo = Tododatabase.choose db.Tododatabase.open_todos in
	let closed = Todo.close todo (get_today ()) in
	Tododatabase.add_closed closed db;
	if not (Todo.is_repeated todo) then
		Tododatabase.delete todo db;
	false

let main_menu db = 
	"Main Menu" |$|
	"h" |: "Todo hinzufuegen"            |-> add_todo   db  |%|
	"a" |: "aktuelle Todos anzeigen"     |-> show_todos db  |%|
	"e" |: "Todo als erledigt markieren" |-> close_todo db  |%|
	"b" |: "Programm beenden"            |-> (fun () -> true)
