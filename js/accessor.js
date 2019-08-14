const get_in = m => (f, d) => {
  const t = e => { throw e }
  try { return f(m) }
  catch (e) { return e instanceof TypeError ? d : t(e) }
}

const data = { a: { b: { c: 'd' } } }

console.log(get_in(data)(_ => _.a.b.c)) // d
console.log(get_in(data)(_ => _.a.x.x)) // undefined
console.log(get_in(data)(_ => _.a.x.x, 'apple')) // 'apple'

const safeData = get_in(data)
console.log(safeData(_ => _.a.b.c)) // d
console.log(safeData(_ => _.a.x.x)) // undefined
