const http = require("http");
const fs = require("fs");
http
    .createServer((req,res) => {
        console.log(req.url);
        const html = fs.readFileSync("./index.html");
        res.writeHeader(200, { "Content-Type": "text/html" });
        res.write(html);
        res.end();
    })
    .listen(3000);
