open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date make_service sp =
	let htmlhead = title (pcdata "Datum ung�ltig") in
	let content = 
		[
			div ~a:[a_class ["error"]] [h1 [pcdata "Angegebenes Datum ist ung�ltig"]]
		]
	in
	make_service sp htmlhead content
	
let post_error make_service sp =
	let htmlhead = title (pcdata "Keine Daten f�r diese Aktion empfangen!") in
	let content = 
		[
			div ~a:[a_class["error"]] [h1 [pcdata "Keine Daten f�r diese Aktion empfangen!"]]
		]
	in
	make_service sp htmlhead content