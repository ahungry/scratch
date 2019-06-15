// port: 12345

const http = require('http')

console.log('Running on port 12345')

//create a server object:
http.createServer(function (req, res) {
  const sleep = req.url.slice(1)
  let wait = sleep

  // Simulating other http codes
  if (sleep > 100) {
    res.statusCode = sleep
    wait = 1
  }

  console.log({ wait, sleep })

  setTimeout(() => {
    res.setHeader('Access-Control-Allow-Origin', '*')
    res.write(JSON.stringify({ sleep }))
    res.end() //end the response
  }, wait * 1e3)

}).listen(12345) //the server object listens on port 8080
