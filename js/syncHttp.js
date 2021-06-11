// Just a test to do a synchronous http call
const http = require('http')

void async function main () {
  const data = await new Promise((resolve, reject) => {
    http
      .request('http://example.com', res => {
        res.on('data', resolve)
        res.on('error', reject)
      })
      .on('error', reject)
      .end('')
  })
  console.log(data.toString())
}()
