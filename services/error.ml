open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date =
	let htmlhead = title (pcdata "Datum ung�ltig") in
	let content = 
		[
			div ~a: [a_class ["error"]] [h1 [pcdata "Angegebenes Datum ist ung�ltig"]]
		]
	in
	Service_base.make_page htmlhead content