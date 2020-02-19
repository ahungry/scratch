const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const range = n => [...Array(n).keys()]
const fetch = n => sleep(1000).then(() => `Fetched record: ${n}`)
Promise.all(range(10000).map(fetch))
  .then(console.log)

// 100k = 1.54 seconds
// 10k = 1.164 seconds
