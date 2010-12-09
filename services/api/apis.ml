let register_all db =
	ignore (Todoapi.make db);
	ignore (Todospec.make db)