// Handle a shared queue between multiple sites/origins.

const ss = sessionStorage
const _get = (s, d) => ss.getItem(s) ? JSON.parse(ss.getItem(s)) : d
const _set = (s, x) => ss.setItem(s, JSON.stringify(x))
const add = x => {
  let queue = _get('queue', [])
  queue.push(x)
  _set('queue', queue)
}
const del = (channel, id) => {
  const events = _get('queue')
  _set('queue', events.filter(event => !(event.channel === channel && event.m.id === id)))
}
const pub = (channel, m) => {
  document.body.style.backgroundColor = m.color
  add({ channel, m })
  console.log({ channel, m })
  setTimeout(() => {
    document.body.style.backgroundColor = '#fff'
  }, 1e2)
}
const subs = []
const sub = (channel, f) => subs.push({ channel, f })
const poll = () => {
  const events = _get('queue')
  if (undefined === events) return
  events.forEach(event => {
    const { channel, m } = event
    let maybeDelete = () => undefined
    subs.forEach(s => {
      if (s.channel == channel) {
        document.body.style.backgroundColor = m.color
        s.f(m)
        maybeDelete = () => del(channel, m.id)
        setTimeout(() => {
          document.body.style.backgroundColor = '#fff'
        }, 1e2)
      }
    })
    maybeDelete()
  })
}
setInterval(poll, 1e2)

const uuid = () => {
  const n = Math.floor(Date.now() / Math.random(1e3) * 1e3)
  const s = String(n) + String(n)
  let r = ''
  for (let i = 0; i < s.length; i++) {
    r += String.fromCharCode('7' + s[i])
  }

  return r
}
