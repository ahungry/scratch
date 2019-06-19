const node = document.getElementById('out')

function out (...rest) {
  console.log(rest)
  node.innerHTML += '<br><br>' + JSON.stringify(rest)
}

// Generic wrapper to get some Json for instance...
async function getJson (opts) {
  out('Begin request...', opts)
  const { sleep } = opts
  // const response = await fetch(`http://127.0.0.1:12345/${sleep}`)
  const ts = (new Date()).getTime()
  const response = await fetch(`http://127.0.0.1:12345/blockme?t=${ts}`)

  if (!response.ok) {
    throw new Error('Http failure')
  }

  const json = await response.json()
  out('Finished request...', opts)

  return json
}

const RETRY_COUNT = 6
const TIMEOUT = 1

async function getJsonWithRetries () {
  let maybe = undefined

  // Lets make a loop of 3 retries maybe.
  for (let i = 0; i < RETRY_COUNT; i++) {
    try {
      maybe = await new Promise(async (resolve, reject) => {
        // However, we want to reject if it takes too long.
        const canceller = setTimeout(_ => reject('Too slow...move on'), TIMEOUT * 1000)

        // So, we would want the first resolved to be kept, and another to be discarded.
        const pause = [10, 502, 503, 404, 1, 5][i]

        // Fire off a request, but do not wait for it.
        // The clearTimeout calls may not be necessary either.
        getJson({ sleep: pause })
          .then(result => {
            clearTimeout(canceller)
            out('Finally received a response...')
            resolve(result)
          })
          .catch(reason => {
            clearTimeout(canceller)
            out('Finally received a reject...')
            reject(reason.toString())
          })
      })

      // If it worked, break out.
      return maybe
    } catch (reason) {
      // Let it continue and try again.
      out('The request was garbage...', { reason })
    }
  }

  throw new Error('Could never resolve any requests in the timeout.')
}

getJsonWithRetries().then(r => out('End result is: ', r))
