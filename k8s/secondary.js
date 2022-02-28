var http = require('http')

// Intentionally running a stateful thing (no-no) to illustrate
// the load balancing feature of this.
var counter = 0

var handleRequest = function(request, response) {
  counter++
	console.log('Received request for URL: ' + request.url, { counter })
	response.writeHead(200)
	response.end(JSON.stringify({ counter }))
}

var www = http.createServer(handleRequest)
www.listen(8080)
