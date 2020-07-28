var express = require("express");
var app = express();
var server = require("http").createServer(app);
var io = require("socket.io")(server);

server.listen(54321);

var aschJS = require('asch-js');
var type = 3;
var fee = String(0.1 * 100000000);

app.get("/sign", function(req, res){
	var symbol = req.query.symbol;
	var amount = req.query.amount;
	var toaddr = req.query.toaddr;
	var secret = decodeURI(req.query.secret);
	var options = {fee: fee, type: type, args: JSON.stringify([symbol, amount, toaddr])};
	var transaction = aschJS.dapp.createInnerTransaction(options, secret);
	res.send(JSON.stringify(transaction));
})

