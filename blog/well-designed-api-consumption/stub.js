const p = require('process')

// function getIp () {
//   return fetch('http://httpbin.org/ip').then(res => res.json())
// }

// getIp()
//   .then(console.log)

// More robust sample
const fetch = require('node-fetch')

const compose = (f, g) => (...args) => f(g.apply(g, args))
const _throw = s => { throw new Error(s) }
const assert = message => x => { return false === Boolean(x) ? _throw(message) : undefined }
const isIpFormat = s => /^\d+\.\d+\.\d+\.\d+/.test(s)
const assertIpFormat = compose(assert('Invalid IP format'), isIpFormat)

class IpModel {
  constructor (json) {
    this.unserialize(json)
  }

  setIp (ip) {
    assertIpFormat(ip)
    this.ip = ip
  }

  unserialize (json) {
    const tmp = JSON.parse(json)
    this.setIp(tmp.origin)
  }
}

const fakeResponse = _ => JSON.stringify({ origin: 'xxx' })
const getIp = _ => fetch('http://httpbin.org/delay/10').then(res => res.json()).then(JSON.stringify)
const getIpWithTimeout = _ => {
  return new Promise((resolve, _) => {
    const to = setTimeout(_ => {
      return resolve(fakeResponse())
    }, 1e3)
    getIp().then(response => { clearTimeout(to); return resolve(response) })
  })
}
const makeIpModel = json => new IpModel(json)
const getIpModel = _ => getIpWithTimeout().then(makeIpModel)

getIpModel()
  .then(console.log)
  .catch(console.log)
  .then(_ => p.exit())
