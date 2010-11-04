open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date =
	html 
		(head (title (pcdata "Datum ungültig")) [])
	 	(body [
			div [ h1 [pcdata "Angegebenes Datum ist ungültig"]]
		])