let register_all db =
	ignore (Todoapi.make_data db);
	ignore (Todoapi.make_spec db);
	ignore (Todoactions.make_addaction db)