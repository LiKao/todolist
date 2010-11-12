open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date make_service sp =
	let htmlhead = title (pcdata "Datum ungültig") in
	let content = 
		[
			div ~a: [a_class ["error"]] [h1 [pcdata "Angegebenes Datum ist ungültig"]]
		]
	in
	make_service sp htmlhead content