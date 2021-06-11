// Just a test to do a synchronous http call
const http = require('http')

// see: https://www.npmjs.com/package/sleep
async function doAsyncRequest (url) {
  return new Promise((resolve, reject) => {
    http
      .request(url, res => {
        res.on('data', resolve)
        res.on('error', reject)
      })
      .on('error', reject)
      .end('')
  })
}

function doSyncRequest (url, f) {
  let data = null

  doAsyncRequest(url, f).then(x => { data = x })

  while (!data) {
    require('deasync').sleep(100)
  }

  return data.toString()
}

// void async function main () {
void function main () {
  console.log('Begin')
  // await doAsyncRequest('http://httpbin.org/ip')
  const x = doSyncRequest('http://httpbin.org/ip', x => x)
  console.log('data was: ', { x })
  console.log('after req')
  console.log('after sleep')
  console.log('End')
}()
