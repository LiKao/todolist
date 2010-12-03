function ReLoad(target) {
  request = new XMLHttpRequest();
  request.open("GET",target,false);
  request.send();
  var content = document.getElementById('content');
  content.innerHTML = request.responseText;
}