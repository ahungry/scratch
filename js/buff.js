// const rows = ['apple', 'orange']
// const buf = new Uint32Array(Buffer.from(rows.join('\x00'), 'binary'))

// console.log(rows.join('\0'))
// console.log(buf)
// console.log('\x00')
// console.log('\x52')

// // buf.set(0, 0x0)
// buf[0] = 0x0
// console.log(buf)

// Pads to an N byte network endian length packet,
// ex: 255 -> 0x000000FF
function makeFixedLen (n, len = 4) {
  // const packet = Buffer.alloc(4)
  const xs = new Uint8Array(len)
  // xs[0] = size >>> 24 // & 0xFF
  // xs[1] = size >>> 16 // & 0xFF
  // xs[2] = size >>> 8 // & 0xFF
  // xs[3] = size & 0xFF
  for (let i = 0; i < len; i++) {
    xs[i] = n >>> (len * 8 - ((i + 1) * 8))
  }

  return xs
}

function unmakeFixedLen (xs) {
  let n = 0

  for (let i = 0; i < xs.length; i++) {
    n += xs[i] << (xs.length * 8 - ((i + 1) * 8))
  }

  return n
}

console.log(makeFixedLen(255))
console.log(makeFixedLen(100))
console.log(makeFixedLen(1000000000))

console.log(unmakeFixedLen(makeFixedLen(1234)))
