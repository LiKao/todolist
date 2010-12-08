let xml_of_named value teipe val_to_string val_to_name =
	[Xml.Element (teipe,[], [Xml.PCData (val_to_string value)]);
	 Xml.Element ("name",[],[Xml.PCData (val_to_name   value)])]