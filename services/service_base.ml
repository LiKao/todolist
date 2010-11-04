open Lwt
open XHTML.M
open Eliom_predefmod.Xhtml


let make_page htmlhead content =
	return (
		html 
		(head htmlhead []) 
		(body
			[div ~a:[a_id "navigation"] []; 
		 	 div ~a:[a_id "content"] content
			]
		)
	)
	 