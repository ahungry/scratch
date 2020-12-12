function unmakeFixedLen (xs) {
  let n = 0

  for (let i = 0; i < xs.length; i++) {
    n += xs[i] << (xs.length * 8 - ((i + 1) * 8))
  }

  return n
}

// Sample input for a PostgreSQL input of:
// Type: Parse, Length: 67, Statement: pdo_stmt_00000001,
// Query: \n-- this is a comment\nSELECT * FROM basket
// Parameters: 0
const input = new Uint8Array([
  80, 0, 0, 0, 67, 112, 100, 111, 95, 115, 116, 109,
  116, 95, 48, 48, 48, 48, 48, 48, 48, 49, 0, 10,
  45, 45, 32, 116, 104, 105, 115, 32, 105, 115, 32, 97,
  32, 99, 111, 109, 109, 101, 110, 116, 10, 83, 69, 76,
  69, 67, 84, 32, 42, 32, 70, 82, 79, 77, 32, 98,
  97, 115, 107, 101, 116, 0, 0, 0, 83, 0, 0, 0,
  4
])

const len = unmakeFixedLen(input.slice(1, 5))
const txt = input.slice(5, 5 + len)
console.log(Buffer.from(txt, 'binary').toString('ascii').split('\x00'))

// Given a Uint8Array of bytes, return the query
function getQueryFromPs (uint8_arr) {
  const len = unmakeFixedLen(uint8_arr.slice(1, 5))
  const txt = uint8_arr.slice(5, 5 + len)

  return Buffer.from(txt, 'binary').toString('ascii').split('\x00')[1]
}

console.log(getQueryFromPs(input))
