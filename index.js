const http = require("http");
const fs = require("fs");


var server = http.createServer();


server.on('request',(req, res) => {
    console.log(req.url);
    var html;

    switch (req.url) {
    case '/elm.js':
        html = fs.readFileSync("./elm.js");
        res.writeHeader(200, {'Content-Type' : 'text/javascript'});
        break;

    default:
        html = fs.readFileSync("./index.html");
        res.writeHeader(200, {'Content-Type' : 'text/html'});
        break;
    }

    res.write(html);
    res.end();

});

server.listen(3000);
