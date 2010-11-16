function IsLeapyear(year) {
  if (year%4 == 0 ){
    return true;
  }
  else {
    return false;
  }
}
 
function DaysOfMonth(month,year) {
  leapyear = IsLeapyear(month);
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