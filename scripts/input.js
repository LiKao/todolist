function skiptext(xml) {
  var i = 0;
  while (xml.childNodes[i].nodeName == "#text") i++;
  return xml.childNodes[i];
}

function Make_Form(element,xml) {
  var formElement = document.createElement("form");
  element.innerHTML = "";
  element.appendChild(formElement);
  var spec = xml.childNodes[0];
  if(spec.nodeName=="spec"){
  	Make_All(formElement,skiptext(spec));
  }
}

function Make_All(element,spec) {
  alert("found node with name: " + spec.nodeName);
  switch(spec.nodeName) {
    case "record":
      return Make_Record(element,spec);
      break;
     case "variant":
       return Make_Variant(element,spec);
       break;
     case "input":
       return Make_Input(element,spec);
       break;
   }		
}

function Make_Record(element,xml) {
  var recordElement = document.createElement("div");
  recordElement.className = "record";
  element.appendChild(recordElement);
  var children = xml.childNodes;
  for(var i=0; i<children.length; i++) {
    if(children[i].nodeName == "record_field") {
      /* Record fields only have one child */
      Make_All(recordElement,skiptext(children[i]));
    }
  }
  return recordElement;
}

function Make_Variant(element,xml) {
  var variantElement = document.createElement("div");
  variantElement.className = "variant";
  element.appendChild(variantElement);
  
  var selectElement = document.createElement("select");
  variantElement.appendChild(selectElement);
	
  var children = xml.childNodes;
  for(var i=0; i<children.length; i++) {
    if(children[i].nodeName == "variant_entry") {
      var name = children[i].getAttribute("name");
      var variantEntry = new Option(name,i);
      selectElement.add(variantEntry); 
    }
  }
  return variantElement;
}

function Make_Input(element,xml) {
  var inputElement = document.createElement("div");
  inputElement.className = "input";
  element.appendChild(inputElement);
  
  var textElement = document.createElement("input");
  textElement.type = "text";
  inputElement.appendChild(textElement);
  
  return inputElement;
}