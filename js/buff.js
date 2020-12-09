const rows = ['apple', 'orange']
const buf = new Uint32Array(Buffer.from(rows.join('\x00'), 'binary'))

console.log(rows.join('\0'))
console.log(buf)
console.log('\x00')
console.log('\x52')

// buf.set(0, 0x0)
buf[0] = 0x0
console.log(buf)
