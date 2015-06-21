
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
	input.value = JSON.stringify(params[key]);
	form.appendChild(input);
    });
    form.submit();
}

function launchDemo(abi) {
    submitPost("/transactionDemo.html", {"abi": abi});
}

