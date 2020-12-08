// most built with wireshark packet analysis since I can't seem to
// find useful byte docs for postgres
// filter in WS: tcp.port in { 5432, 5433 }

const net = require('net')
const port = 5433

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
function makeRowDescPacket (rowHeaders) {
  const packetLength = 0x1E // TODO: Compute this
  const fieldCount = rowHeaders.length
  const type = [0x54]
  const length = [0x0, 0x0, 0x0, packetLength]
  const fieldCountPacket = [0x0, fieldCount]
  const payData = new Uint32Array(Buffer.from(rowHeaders[0], 'binary'))
  const buf = Buffer.from([
    ...type, ...length, ...fieldCountPacket,
    ...payData, 0x0,
    0x0, 0x0, 0x40, 0x6,
    0x0, 0x2,
    0x0, 0x0, 0x4, 0x13,
    0xff, 0xff,
    0x0, 0x0, 0x0, 0x36,
    0x0, 0x0], 'binary')

  return buf
}

function makeRowPacket (rows) {
  const packetLength = 0x0F // TODO: Compute this
  const fieldCount = rows.length
  const type = [0x44]
  const length = [0x0, 0x0, 0x0, packetLength]
  const fieldCountPacket = [0x0, fieldCount]
  const payData = new Uint32Array(Buffer.from(rows[0], 'binary'))
  const buf = Buffer.from([
    ...type, ...length, ...fieldCountPacket,
    // Length of the row data
    0x0, 0x0, 0x0, payData.length,
    // The actual row data
    ...payData,
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
      socket.write(makeRowDescPacket(['fruit']))
      socket.write(makeRowPacket(['apple']))
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
      // TODO: Get working
      // A PDO statement
      console.log('PDO prepared query time!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      socket.write(authPacket())
      socket.write(makeReadyForQuery())
      // TODO: Maybe add missing Type: Parse portion
      socket.write(syncPacket())
      // socket.write(makeRowDescPacket(['fruit']))
      // socket.write(makeRowPacket(['apple']))
      // socket.write(makeCommandCompletePacket('SELECT 1'))
      // socket.write(makeReadyForQuery())
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
