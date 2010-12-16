function IsLeapyear(year) {
  return (year%4 == 0);
}

function DaysOfMonth(year,month) {
  var leapyear = IsLeapyear(year);
  var days = new Array();
  days[0] = 31
  if(leapyear) {
    days[1] = 29; 
  }
  else{
    days[1] = 28;
  }
  days[2] = 31;
  days[3] = 30;
  days[4] = 31;
  days[5] = 30;
  days[6] = 31;
  days[7] = 31;
  days[8] = 30;
  days[9] = 31;
  days[10] = 30;
  days[11] = 31;
  return days[month];
}

function getMonthStartday(year,month) {
  var d = new Date(year,month,1);
  return d.getDay();
}

//TODO: Generate this from inside the server

var dayNames = new Array();
dayNames[0] = "So";
dayNames[1] = "Mo";
dayNames[2] = "Di";
dayNames[3] = "Mi";
dayNames[4] = "Do";
dayNames[5] = "Fr";
dayNames[6] = "Sa";

var monthNames = new Array();
monthNames[ 0] = "Januar";
monthNames[ 1] = "Februar";
monthNames[ 2] = "März";
monthNames[ 3] = "April";
monthNames[ 4] = "Mai";
monthNames[ 5] = "Juni";
monthNames[ 6] = "Juli";
monthNames[ 7] = "August";
monthNames[ 8] = "September";
monthNames[ 9] = "Oktober";
monthNames[10] = "November"
monthNames[11] = "Dezember";

function Calendar(element,_dateVal,mustReset) {

  /**** Argument checking ****/
  
  if(typeof(mustReset) == "undefined")
    mustReset = false;

  /**** Object Fields ****/
  
  function dayType(_date,_element) {
    this.date    = _date;
    this.element = _element;
  }
  
  var days = new Array();
  var dayRows = new Array();
  var dateVal = _dateVal;
  //displayed dates might differ from dateVal, so we have to store month and year extra
  var yearVal = dateVal.getFullYear();
  var monthVal = dateVal.getMonth();
  var selectedCell;    
    
  /**** Handlers ****/
 
  // needed because of dynamic binding
  var self = this;
 
  function onClickHandler(element) {
	//find the element that this click originated from and set the date
	for(var i=0; i< days.length; i++) {
	  if(days[i].element==element) {
	    dateVal = days[i].date;
	    break;
	  }
	}
    self.onchange();
  }
  
  this.onchange = function() {
    return true;
  }
  
  function onMonthChangeHandler(month) {
    monthVal = month;
    WriteCalendar(yearVal,monthVal);
    RegisterHandlers();
  	if(mustReset){
  	  var day = dateVal.getDate();
  	  if(day > DaysOfMonth(yearVal,monthVal))
  	  	day = DaysOfMonth(yearVal,monthVal);
  	  days[day-1].element.className = "selected";
  	  selectedCell = days[day-1].element;
  	  dateVal = new Date(yearVal,monthVal,day);
      self.onchange();
    }
    else{
      selectedCell = null;
    }
  }
  
  function onYearChangeHandler() {
    var pattern = /^\d+$/;
    var year = this.value;
    if(!year.match(pattern)) {
      this.value = yearVal;
      return;
    }
    yearVal = year;
    
    WriteCalendar(yearVal,monthVal);
    RegisterHandlers();
    
    if(mustReset){
      var day = dateVal.getDate();
      if(day > DaysOfMonth(yearVal,monthVal))
  	  	day = DaysOfMonth(yearVal,monthVal);
  	  days[day-1].element.className = "selected";
  	  selectedCell = days[day-1].element;
  	  dateVal = new Date(yearVal,monthVal,day);
      self.onchange();
    }
    else{
      selectedCelle = null;
    }
  }
  
  function makeDays(year,month) {
    var numDays = DaysOfMonth(year,month);
    days.length = 0;
    for(var i = 1; i <=numDays; i++) {
      var d = new Date(year,month,i);
      var dayElement = document.createElement("div");
      dayElement.id = "calendarday" + i;
      dayElement.appendChild(document.createTextNode(i));
      days[i-1] = new dayType(d,dayElement);
    }
  }
  
  /**** Output of the calendar ****/
  
  var yearField      = document.createElement("input");
  yearField.type = "text";
  yearField.onchange = onYearChangeHandler;
  var monthSelection = document.createElement("select");
  monthSelection.onchange = function() {
    var month = this.options[this.selectedIndex].value;
    onMonthChangeHandler(month);
  }
  var tableElement   = document.createElement("table");
  
  // create all months
  for(var i = 0; i < monthNames.length; i++) {
    var opt = new Option(monthNames[i],i);
    monthSelection.add(opt);
  }
  
  // create the header for the table
  var headerRow = document.createElement("tr");
  for(var i = 0; i < dayNames.length; i++) {
    var header = document.createElement("th");
    header.appendChild(document.createTextNode(dayNames[i]));
    headerRow.appendChild(header);
  }
  tableElement.appendChild(headerRow);
 
  element.innerHTML = "";
  element.appendChild(monthSelection);
  element.appendChild(yearField);
  element.appendChild(tableElement);
  
  function WriteCalendar(year,month) {
    // remove all old rows
    for(var i=0;i<dayRows.length;i++) {
      tableElement.removeChild(dayRows[i]);
    }
    dayRows.length = 0;
    var startday = getMonthStartday(year,month);
    dayRows[0] = document.createElement("tr");
    // skip some table elements in the first row
    for(var i = 0; i < startday; i++) {
      var skipElement = document.createElement("td");
      dayRows[0].appendChild(skipElement);
    }
    
    makeDays(year,month);
    
    for(var i = 0; i < days.length; i++) {
	  if(days[i].date.getDay() == 0)
	    dayRows.push(document.createElement("tr"));
	  var dataField = document.createElement("td");
	  dataField.appendChild(days[i].element);
      dayRows[dayRows.length-1].appendChild(dataField);
    }
    
    // write the new rows
    for(var i = 0; i < dayRows.length; i++) {
      tableElement.appendChild(dayRows[i]);
    }
  }
  
  function RegisterHandlers() {
    for(var i = 0; i < days.length; i++) {
      days[i].element.onclick = function()
        {
          if(typeof(selectedCell != "undefined"))
            selectedCell.className = "";
          selectedCell = this;
          this.className = "selected";
          onClickHandler(this);
          return true;
        };
    }
  }
  
  function SetDefaults() {
    yearField.value = dateVal.getFullYear();
    monthSelection.selectedIndex= dateVal.getMonth();
    selectedCell = days[dateVal.getDate()-1].element;
    selectedCell.className = "selected";
  }
  
  WriteCalendar(dateVal.getFullYear(),dateVal.getMonth());
  RegisterHandlers();
  SetDefaults();
  

  /**** Accesor functions ****/
  this.getDate = function() {
    return dateVal;
  }
   
}


