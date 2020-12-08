const net = require('net')
const port = 5433

function authPacket () {
  const authReqType = [0x52]
  const length8 = [0x0, 0x0, 0x0, 0x8]
  const authSuccess = [0x0, 0x0, 0x0, 0x0]
  const buf = Buffer.from([...authReqType, ...length8, ...authSuccess], 'binary')

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

    // New connection request
    if (0x52 === buf[3]) {
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
      socket.write(makeReadyForQuery())
    }
    else if (0x88 === buf[0] || 88 === buf[0]) {
      console.log('Client disconnected!')
    }
    else {
      console.log('Unknown time!')
      console.log(`Data received from client: ${chunk.toString()}`)
      console.log(buf)
      // socket.write(authPacket())
      // socket.write(makeStatusPacket('application_name', 'psql'))
      socket.write(makeReadyForQuery())
    }
  })

  socket.on('end', function () {
    console.log('Closing connection with the client')
  })

  socket.on('error', function (err) {
    console.log(`Error: ${err}`)
  })
}).listen(port)
