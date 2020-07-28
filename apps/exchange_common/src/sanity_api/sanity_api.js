var express = require("express");
var app = express();
var server = require("http").createServer(app);
var io = require("socket.io")(server);

server.listen(55555);

const url = "mongodb://localhost:27017";
const dbName = "exchange_wallet_common";
var mongo = require("mongodb").MongoClient;
var block_scan = null;

mongo.connect(url, {useNewUrlParser:true},
    function(err, client){
        if(err){
            console.log("\033[96m - \033[39m failed connect to mongodb");
            throw err;
        }
        block_scan = client.db(dbName).collection("block_scan");
        console.log("\033[96m + \033[39m connected to mongodb");
    });

app.get("/sanity", function(req, res){
    var chain = req.query.chain;
    block_scan.findOne({"_id": chain}, function(err, doc){
    	if (err) return next(err);
    	res.send(JSON.stringify(doc));
    })
})

