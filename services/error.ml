open XHTML.M
open Eliom_predefmod.Xhtml

let invalid_date =
	html 
		(head (title (pcdata "Datum ung�ltig")) [])
	 	(body [
			div [ h1 [pcdata "Angegebenes Datum ist ung�ltig"]]
		])