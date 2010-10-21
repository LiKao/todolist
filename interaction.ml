open Printf

exception NoSelection

type 'a choice = {name  : string;
                  value : 'a}

type shortcut = char
							
type 'a menu = {title   : string;
                choices : (shortcut * ('a choice)) list}
								
let (|$|) title shortcut = (shortcut,{title = title;choices =[]})
let (|:)  (shortcut,menustub) name = (shortcut,name,menustub)
let (|->) (shortcut,name,menustub) value = {title = menustub.title;
                                           choices = menustub.choices @ 
																					   [(shortcut,{name = name;value = value})]}
let (|%|) menustub shortcut = (shortcut,menustub)																						
		

let display_menu menu =
	printf "\n\n%s\n" menu.title;
	printf "------------------------\n";
	List.iter (fun (shortcut,choice) -> printf "%c: %s\n" shortcut choice.name) menu.choices;
	flush stdout;
	let rec loop () =
		let selection =
			try
				let line = input_line stdin in
				line.[0]
			with Invalid_argument _ ->
				raise NoSelection
		in
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
	