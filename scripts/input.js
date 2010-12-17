function skiptext(xml) {
  var i = 0;
  while (xml.childNodes[i].nodeName == "#text") i++;
  return xml.childNodes[i];
}

function formCreator(element,xml,target,name) {

  var formElement = document.createElement("form");
  formElement.method = "post";
  formElement.action = target;
  var xmlField = document.createElement("textarea");
  xmlField.name = name;
  formElement.appendChild(xmlField);
  var submitButton = document.createElement("input");
  submitButton.type = "submit";
  submitButton.value = "Eintragen";
  formElement.appendChild(submitButton);
  var inputElement = document.createElement("div");
  element.innerHTML = "";
  element.appendChild(inputElement);
  element.appendChild(formElement);

  var responseDoc = document.implementation.createDocument("","response",null);
  var s = new XMLSerializer();
  
  
  /************************ update method *****************/
  function update() {
    xmlField.value = s.serializeToString(responseDoc);
  }

  function Make_All(element,spec,response) {
    switch(spec.nodeName) {
      case "record":
        Make_Record(element,spec,response);
        break;
       case "variant":
         Make_Variant(element,spec,response);
         break;
       case "input":
         Make_Input(element,spec,response);
         break;
       case "intrange":
         Make_Intrange(element,spec,response);
         break;
       case "value":
         Make_Value(element,spec,response);
         break;  
       case "date":
         Make_Date(element,spec,response);
    }		
  }

  /******************** Make_Record method *****************/
  function Make_Record(element,spec,response) {
    var recordElement = document.createElement("div");
    recordElement.className = "record";
    element.appendChild(recordElement);
    
    
    var recordResponse = responseDoc.createElement(spec.getAttribute("fieldname"));
    response.appendChild(recordResponse);
    
    var children = spec.childNodes;
    for(var i=0; i<children.length; i++) {
      if(children[i].nodeName == "record_field") {
        /* Record fields only have one child */
        Make_All(recordElement,skiptext(children[i]),recordResponse);
      }
    }
  }
  
  /******************** Make_Variant method *****************/
  function Make_Variant(element,spec,response) {
    var variantElement = document.createElement("div");
    variantElement.className = "variant";
    element.appendChild(variantElement);
    
    var selectElement = document.createElement("select");
    variantElement.appendChild(selectElement);
    
    var entryData = document.createElement("div");
    entryData.className = "entryData";
    variantElement.appendChild(entryData);
    
    var variantResponse = responseDoc.createElement(spec.getAttribute("fieldname"));
    response.appendChild(variantResponse);
  	
    var children = spec.childNodes;
    for(var i=0; i<children.length; i++) {
      if(children[i].nodeName == "variant_entry") {
        var name = children[i].getAttribute("name");
        var variantEntry = new Option(name,i);
        selectElement.add(variantEntry); 
      }
    }
    
    var entry = selectElement.options[selectElement.selectedIndex];
    var entryNum = entry.value;
    var entryNode = responseDoc.createElement(children[entryNum].getAttribute("fieldname"));
    variantResponse.appendChild(entryNode);
    if(children[entryNum].childNodes.length > 0) {
  	  sub_html = Make_All(entryData,skiptext(children[entryNum]),entryNode);
  	}
    
    selectElement.onchange = function () {
  	  entryData.innerHTML = "";
      var entry = this.options[this.selectedIndex];
      var entryNum = entry.value;
      
      variantResponse.removeChild(entryNode);
      entryNode = responseDoc.createElement(children[entryNum].getAttribute("fieldname"));
      variantResponse.appendChild(entryNode);
      
      if (children[entryNum].childNodes.length > 0)
        sub_html = Make_All(entryData,skiptext(children[entryNum]),entryNode);
      update();
    }
  }
  
  
  /******************** Make_Input method *****************/
  function Make_Input(element,spec,response) {
    var inputElement = document.createElement("div");
    inputElement.className = "input";
    element.appendChild(inputElement);
    
    var textElement = document.createElement("input");
    textElement.type = "text";
    inputElement.appendChild(textElement);
    
    var textResponse = responseDoc.createElement(spec.getAttribute("fieldname"));
    response.appendChild(textResponse);
    var textValue = responseDoc.createTextNode(textElement.value);
    textResponse.appendChild(textValue);
    
    textElement.onchange = function() {
      textValue.nodeValue = this.value;
      update();
    }
  }
  
  /******************** Make_Intrange method *****************/
  function Make_Intrange(element,spec,response) {
    var min = spec.getAttribute("min");
    var max = spec.getAttribute("max");
    
    var intrangeElement = document.createElement("div");
    intrangeElement.className = "intrange";
    element.appendChild(intrangeElement);
    
    var selectElement = document.createElement("select");
    intrangeElement.appendChild(selectElement);
    for(var i = min; i<=max; i++) {
      var opt = new Option(i,i);
      selectElement.add(opt);
    }
    
    var intrangeResponse = responseDoc.createElement(spec.getAttribute("fieldname"));
    response.appendChild(intrangeResponse);
    var intrangeResponseText = responseDoc.createTextNode(min);
    intrangeResponse.appendChild(intrangeResponseText);
    
    selectElement.onchange = function() {
      intrangeResponseText.nodeValue = this.options[this.selectedIndex].value;
      update();
    }
  }
  
  /****************** Make_Date method ****************/
  function Make_Date(element,spec,response) {
    var dateElement = document.createElement("div");
    dateElement.className = "date";
    var calendarObj = new Calendar(dateElement, new Date(),true);
    element.appendChild(dateElement);
    
    var yearNode = responseDoc.createElement("year");
    var yearText = responseDoc.createTextNode(calendarObj.getDate().getFullYear());
    yearNode.appendChild(yearText);
    var monthNode = responseDoc.createElement("month");
    var monthText = responseDoc.createTextNode(calendarObj.getDate().getMonth());
    monthNode.appendChild(monthText);
    var dayNode =  responseDoc.createElement("day");
    var dayText = responseDoc.createTextNode(calendarObj.getDate().getDate());
    dayNode.appendChild(dayText);
    var dateResponse = responseDoc.createElement("date");
    dateResponse.appendChild(yearNode);
    dateResponse.appendChild(monthNode);
    dateResponse.appendChild(dayNode);
    response.appendChild(dateResponse);
        
    calendarObj.onchange = function(){
      yearText.nodeValue = calendarObj.getDate().getFullYear();
      monthText.nodeValue = calendarObj.getDate().getMonth();
      dayText.nodeValue = calendarObj.getDate().getDate();
      update();
    };
    
    update();
  }
  
  /******************** Make_Value method *****************/
  function Make_Value(element,spec,response) {
    response.appendChild(spec.childNodes[0].cloneNode(true));
  }
 
  var spec = xml.childNodes[0];
  if(spec.nodeName=="spec"){
    Make_All(inputElement,skiptext(spec),skiptext(responseDoc));
  }
  
  update(); 	      
  
}



