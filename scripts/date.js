function IsLeapyear(year) {
  return (year%4 == 0);
}
 
function DaysOfMonth(month,year) {
  var leapyear = IsLeapyear(year);
  var days = new Array();
  days[0] = 0;
  days[1] = 31
  if(leapyear) {
    days[2] = 29; 
  }
  else{
    days[2] = 28;
  }
  days[3] = 31;
  days[4] = 30;
  days[5] = 31;
  days[6] = 30;
  days[7] = 31;
  days[8] = 31;
  days[9] = 30;
  days[10] = 31;
  days[11] = 30;
  days[12] = 31;
  return days[month];
}

function WriteCalendar(element,days,startday,callback) {
  var content = "<table><tr> <th>Mo</th> <th>Di</th> <th>Mi</th> <th>Do</th> <th>Fr</th> <th>Sa</th> <th>So</th>"
  
  startday = (startday + 6) % 7;
  var j = 0;
  for (i=0;i<startday;i++){
    if(j%7==0) {
      content = content + "</tr><tr>";
    }
    j++;
    content = content + "<td></td>";
    
  }
  
  for(i=1;i<=days;i++) {
    if(j%7==0) {
      content = content + "</tr><tr>";
    }
    j++;
    content = content + "<td><div id=\"calendarday" + i + "\">" + i + "</div>";  
  }
  content = content + "</tr></table>";
  element.innerHTML = content;
  
  function handler() {
    var c = this.innerHTML;
    callback(parseInt(c));
  }
  for(i=1;i<=days;i++){
    var id = "calendarday" + i;
    var cell = document.getElementById(id);
    cell.onclick = handler;
  }
}