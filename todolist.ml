open Printf

let db = Tododb.load ()
 
let _ = Services.register_all db

