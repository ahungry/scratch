// most built with wireshark packet analysis since I can't seem to
// find useful byte docs for postgres
// filter in WS: tcp.port in { 5432, 5433 }

const net = require('net')
const port = 5433

function makeFixedLen (n, len = 4) {
  // const packet = Buffer.alloc(4)
  const xs = new Uint8Array(len)
  for (let i = 0; i < len; i++) {
    xs[i] = n >>> (len * 8 - ((i + 1) * 8))
  }

  return xs
}

function noSslPacket () {
  const type = [0x4E] // N - NO
  const buf = Buffer.from([...type], 'binary')

  return buf
}

function authPacket () {
  const authReqType = [0x52]
  const length8 = [0x0, 0x0, 0x0, 0x8]
  const authSuccess = [0x0, 0x0, 0x0, 0x0]
  const buf = Buffer.from([...authReqType, ...length8, ...authSuccess], 'binary')

  return buf
}

// TODO: This should probably just mirror the input user chunk for data
function makeParsePacket () {
  const type = [0x50]
  const len = makeFixedLen(45, 4)
  const data = 'pdo_stmt_00000001\0x00'
    + 'SELECT * FROM basket\0x00'
  const params = makeFixedLen(0, 2)

  return Buffer.from([
    ...type,
    ...len,
    ...new Uint8Array(Buffer.from(data, 'binary')),
    ...params,
  ])
}

function syncPacket () {
  const typ = [0x53]
  const len = [0x0, 0x0, 0x0, 0x4]
  const buf = Buffer.from([...typ, ...len], 'binary')

  return buf
}

function makeStatusPacket (key, val) {
  const authStatusType = [0x53]
  const len = [0x0, 0x0, 0x0, key.length + 1 + val.length + 1 + 4]
  console.log('Len is: ', len)
  const payKey = new Uint32Array(Buffer.from(key, 'binary'))
  const payVal = new Uint32Array(Buffer.from(val, 'binary'))
  const buf = Buffer.from([...authStatusType, ...len, ...payKey, 0x0, ...payVal, 0x0], 'binary')

  // console.log('bin buf is: ', buf)

  return buf
}

// After the row desc, it sends the data
// with 'fruit' as the input, these vals are correct
function makeRowDescPacket (rows) {
  // type modifier is a 4 byte wide
  // 36 (54) is text
  // while -1 (0xFFFFFFFF) is int
  const textType = '\x00\x00\x00\x36'
  const intType = '\xFF\xFF\xFF\xFF'

  // 18 bytes of useful info such as oid, column index etc., can we ignore?
  // it comes after each piece of data in the rows

  // Format is: name + null (1) + oid (4) + col idx (2) + oid (4) + colLen (2) + type (4) + fmt (2)
  const joinedRows = rows.join(
    `\x00\x00\x00\x40\x06\x00\x02\x00\x00\x04\x13\xff\xff${intType}\x00\x00`
  ) + `\x00\x00\x00\x40\x06\x00\x02\x00\x00\x04\x13\xff\xff${textType}\x00\x00`
  // Is it total size of byte width?
  // 30 = fruit (5) + len (4) + fclen (2) + datalen (4) + ?? (14) + type (1)
  // 51 = 30 + id (2) +

  console.log('here')
  const foo = Buffer.from(joinedRows, 'binary')
  console.log(foo)

  const packetLength = joinedRows.length + 4 + 2
  console.log('packet len is: ', packetLength)
  const fieldCount = rows.length
  const type = [0x54]
  const length = makeFixedLen(packetLength, 4)
  const fieldCountPacket = makeFixedLen(fieldCount, 2)
  const payData = new Uint8Array(Buffer.from(joinedRows, 'binary'))
  const buf = Buffer.from([
    ...type, ...length, ...fieldCountPacket,
    ...payData,
  ], 'binary')

  console.log(buf)

  return buf
}

// This is with 'apple' as the row input
function makeRowPacket (rows) {
  // This seems to be the total byte width other than the 'type', as
  // we test at 15 = apple (5) + len (4) + fclen (2) + datalen (4)
  // const joinedRows = rows.join('\x00')

  // After headers, each row first sends its length in bytes, then the actual payload
  // The length is a 4 width
  let data = new Uint8Array()

  // FIXME: Probably real inefficient, why can't we append to the array?
  for (let i = 0; i < rows.length; i++) {
    data = new Uint8Array([...data, ...makeFixedLen(rows[i].length, 4)])
    data = new Uint8Array([...data, ...Buffer.from(rows[i], 'binary')])
  }

  console.log('Payload data is: ', data)

  const packetLength = data.length + 4 + 2
  const fieldCount = rows.length
  const type = [0x44]
  const length = makeFixedLen(packetLength, 4)
  const fieldCountPacket = makeFixedLen(fieldCount, 2)
  // const payData = new Uint8Array(Buffer.from(joinedRows, 'binary'))
  const buf = Buffer.from([
    ...type, ...length, ...fieldCountPacket,
    ...data,
  ], 'binary')

  return buf
}

function makeCommandCompletePacket (data) {
  const packetLength = 0x0D // TODO: Compute this
  const type = [0x43]
  const length = [0x0, 0x0, 0x0, packetLength]
  let payData = new Uint32Array(Buffer.from(data, 'binary'))
  const buf = Buffer.from([
    ...type, ...length,
    ...payData, 0x0,
  ], 'binary')

  return buf

}

function makeReadyForQuery () {
  const authStatusType = [0x5A]
  const len = [0x0, 0x0, 0x0, 0x5]
  const payload = [0x49]
  const buf = Buffer.from([...authStatusType, ...len, ...payload], 'binary')

  return buf
}

net.createServer(function (socket) {

  socket.on('data', function (chunk) {
    const buf = new Uint32Array(Buffer.from(chunk, 'binary'))
    console.log(buf)

    // On the incoming data from client, node output will print it as decimal,
    // but the postgres info in wireshark are displayed in hex

    // New connection request, or new conn after failed SSL
    if (0x52 === buf[3] || 0x3A === buf[3]) {
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      socket.write(authPacket())
      socket.write(makeStatusPacket('application_name', 'psql'))
      socket.write(makeStatusPacket('client_encoding', 'UTF8'))
      socket.write(makeStatusPacket('DateStyle', 'ISO, MDY'))
      socket.write(makeStatusPacket('integer_datetimes', 'on'))
      socket.write(makeStatusPacket('IntervalStyle', 'postgres'))
      socket.write(makeStatusPacket('is_superuser', 'off'))
      socket.write(makeStatusPacket('server_encoding', 'UTF8'))
      socket.write(makeStatusPacket('server_version', '12.5'))
      socket.write(makeStatusPacket('session_authorization', 'mcarter'))
      socket.write(makeStatusPacket('standard_conforming_strings', 'on'))
      socket.write(makeStatusPacket('TimeZone', 'America/Detroit'))

      socket.write(makeReadyForQuery())
    }
    // Query request
    else if (0x51 === buf[0]) {
      console.log('Query time!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      // socket.write(authPacket())
      // socket.write(makeStatusPacket('application_name', 'psql'))
      socket.write(makeRowDescPacket(['fruit', 'id']))
      socket.write(makeRowPacket(['apple', '1']))
      socket.write(makeRowPacket(['orange', '2']))
      socket.write(makeCommandCompletePacket('SELECT 1'))
      socket.write(makeReadyForQuery())
    }
    else if (0x58 === buf[0]) {
      // Normal connection from terminal
      console.log('Client disconnected!')
    }
    else if (0x27 === buf[3]) {
      // Connection from PDO
      console.log('PDO Client connected!')
      socket.write(authPacket())
      socket.write(makeReadyForQuery())
    }
    else if (0x3D === buf[3]) {
      // The stdin pipe to psql CLI
      console.log('Pipe time!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      socket.write(authPacket())
      socket.write(makeReadyForQuery())
      socket.write(makeRowDescPacket(['fruit']))
      socket.write(makeRowPacket(['apple']))
      socket.write(makeCommandCompletePacket('SELECT 1'))
      // socket.write(makeReadyForQuery())
    }
    else if (0x50 === buf[0]) {
      // PARSE
      // A PDO statement begins here
      console.log('Parse call received!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)

      // socket.write(authPacket())
      // socket.write(makeReadyForQuery())
      socket.write(makeParsePacket())

      // TODO: Maybe add missing Type: Parse portion
      socket.write(syncPacket())
      // socket.write(makeRowDescPacket(['fruit']))
      // socket.write(makeRowPacket(['apple']))
      // socket.write(makeCommandCompletePacket('SELECT 1'))
      // socket.write(makeReadyForQuery())
    }
    else if (0x42 === buf[0]) {
      // PDO Bind
      console.log('Bind call received')
    }
    else if (0x8 === buf[3]) {
      // Client wants SSL
      // https://www.postgresql.org/docs/10/protocol-flow.html
      // 52.2.9. SSL Session Encryption
      console.log('no SSL for you')
      socket.write(noSslPacket())
    }
    else {
      console.log('Unknown time!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      socket.write(authPacket())
      socket.write(makeReadyForQuery())
      socket.write(makeRowDescPacket(['fruit']))
      socket.write(makeRowPacket(['apple']))
      socket.write(makeCommandCompletePacket('SELECT 1'))
      socket.write(syncPacket())
      // socket.write(makeReadyForQuery())
    }
  })

  socket.on('end', function () {
    console.log('Closing connection with the client')
  })

  socket.on('error', function (err) {
    console.log(`Error: ${err}`)
  })
}).listen(port)
