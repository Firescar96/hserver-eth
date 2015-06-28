
var abi=#{rawJS abi}
var contractAddress="#{rawJS contractAddress}"

function submitPost(url, params) {
    var i;
    var form = document.createElement("form");
    form.method = 'post';
    form.action = url;
    form.setAttribute("target", "_blank");
    Object.keys(params).forEach(function (key) {
	var input = document.createElement('input');
	input.type = "text";
	input.name = key;
	if (typeof params[key] == "string") input.value = params[key];
	else input.value = JSON.stringify(params[key]);
	form.appendChild(input);
    });
    form.submit();
}

function launchDemo(abi) {
  submitPost("/transactionDemo.html", {"abi": abi, "contractAddress": contractAddress});
}


function getTransactionResults() {
  $.get("/transactionResult/#{rawJS transactionHash}")
    .done(function(){
      var i, responseText = "";
      var response = JSON.parse(this.responseText);
      for(i = 0; i < response.length; i++) {
        result.value = result.value + response[i].message + "\n";
        details.value = details.value + response[i].details + "\n";
      }
    })
    .fail(function() {
      alert("ERROR!\n" + this.responseText);
    })
}

getTransactionResults();
