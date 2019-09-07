// A great sample for concurrency (completes in 1 second)
// But, what about parallelism?

// lets make a 'sleep' equivalent that resolves in 1 second but does work
const burner = () => {
  return new Promise((resolve, reject) => {
    let i = 0
    let start = Date.now()
    while (Date.now() < (start + 1e3)) {
      i++
    }

    resolve(i)
  })
}

const range = n => [...Array(n).keys()]
const fetch = n => burner().then(() => `Fetched record: ${n}`)
Promise.all(range(100).map(fetch))
  .then(console.log)

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const range = n => [...Array(n).keys()]
const fetch = n => sleep(1000).then(() => `Fetched record: ${n}`)
Promise.all(range(100).map(fetch))
  .then(console.log)
