let make_head title = 
	XHTML.M.head (XHTML.M.title (XHTML.M.pcdata title)) []
	
let make_inner_body f = 
	XHTML.M.body [f ()]