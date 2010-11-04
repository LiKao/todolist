open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date =
	let htmlhead = title (pcdata "Datum ungültig") in
	let content = 
		[
			div ~a: [a_class ["error"]] [h1 [pcdata "Angegebenes Datum ist ungültig"]]
		]
	in
	Service_base.make_page htmlhead content