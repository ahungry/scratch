var http = require('http')

function make_req_to_secondary () {
  return new Promise((resolve, reject) => {
    const requestBody = '';
    const options = {
      host: 'secondary-api',
      port: 12346,
      path: '/',
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': requestBody.length
      }
    };

    const req = http.request(options, (res) => {
      var responseString = ''

      res.on('data', function (data) {
        responseString += data
        // save all the data from response
      });
      res.on('end', function () {
        // print to console when response ends
        resolve(responseString)
      });
    });
    req.write(requestBody)
    req.end()
  })
}

// Intentionally running a stateful thing (no-no) to illustrate
// the load balancing feature of this.
var counter = 0

var handleRequest = async function (request, response) {
  const secondary_res = await make_req_to_secondary()
  counter++
  console.log('Received request for URL: ' + request.url, { counter })
  response.writeHead(200)
  response.end(JSON.stringify({ counter, secondary_res, message: 'Hello' }))
}

console.log('listening on 8080')
var www = http.createServer(handleRequest)
www.listen(8080)
