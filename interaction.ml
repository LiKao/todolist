open Printf

exception No_selection

type 'a choice = {name  : string;
                  value : 'a}

type shortcut = string
							
type 'a menu = {title   : string;
                choices : (shortcut * ('a choice)) list}
								
let (|$|) title shortcut = (shortcut,{title = title;choices =[]})
let (|:)  (shortcut,menustub) name = (shortcut,name,menustub)
let (|->) (shortcut,name,menustub) value = {title = menustub.title;
                                           choices = menustub.choices @ 
																					   [(shortcut,{name = name;value = value})]}
let (|%|) menustub shortcut = (shortcut,menustub)																						
		
let make_choices values printer title =
	let choices = Array.mapi 
	  (fun i value -> (sprintf "%i" i,{name = printer value; value = value}))
		values
	in 
	{title = title; choices = Array.to_list choices} 
		
let display_choice menu =
	printf "\n\n%s\n" menu.title;
	printf "------------------------\n";
	List.iter (fun (shortcut,choice) -> printf "%s:\t%s\n" shortcut choice.name) menu.choices;
	flush stdout;
	let rec loop () =
		let selection = input_line stdin in
		if String.length selection = 0 then raise No_selection;
		try begin
			let res = List.assoc selection menu.choices in 
			res.value
		end
		with Not_found ->
			begin
				printf "Invalid selection.\n";
				flush stdout;
				loop ()
			end
	in loop ()

let rec display_menu menu =
	try display_choice menu
	with No_selection -> display_menu menu
	