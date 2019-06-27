// We have to test these guys further down.
const isScalar = x => !Array.isArray(x)
const allScalar = xs => xs.filter(isScalar).length === xs.length
const wrapArray = xs => xs.map(x => [x])

const tests = []
function suite () {
  console.log('Begin testing...')
  tests.map(t => t.apply(t))
  console.log('Done testing...')
}

function shallBe (fn, expect, ...args) {
  if (allScalar(args)) args = wrapArray(args)

  args.forEach(arg => {
    tests.push(() => {
      const result = fn.apply(fn, arg)

      console.assert(
        result.toString() === expect.toString(), // Hackish fix for array checking atm.
        `Expected: ${String(expect).toString()} but received ${String(result).toString()}
in: ${fn.constructor.name}
  ${fn.toString()} with args: ${arg.toString()}`
      )
    })
  })
}

const test = f => f.shallBe = shallBe.bind(shallBe, f)

// Hey, we can even test our own test suite, inline with our test suite.
test(isScalar)
isScalar.shallBe(true, 1, 'dog', undefined, null, -1, {})
isScalar.shallBe(false, [[]])

test(allScalar)
allScalar.shallBe(true, [[1, 5, 'dog']])
allScalar.shallBe(false, [[1, [], 'dog']])

test(wrapArray)
wrapArray.shallBe([1, 'x'], [[[1, 'x']]])

const isEven = n => n % 2 === 0
test(isEven)
isEven.shallBe(true, 0, 2, 4, 6, 8, 10)
isEven.shallBe(false, 1, 3, -1)

const isOdd = n => !isEven(n)
test(isOdd)
isOdd.shallBe(true, 1, 3, 5)
isOdd.shallBe(false, 2, 4, 8)

const sum = (a, b) => a + b
test(sum)
sum.shallBe(5, [2, 3], [1, 4])
sum.shallBe(10, [5, 5])
sum.shallBe(-10, [-5, -5])

class Foo {
  constructor () {
  }

  bar () {
    return 'dog'
  }
}

shallBe((new Foo()).bar, 'dog', [])
// shallBe((new Foo()).bar, 'cat', [])
// The last one is a fail case - it gives output as such:
// AssertionError [ERR_ASSERTION]: Expected: cat but received dog
// in: Function
//   bar() {
//     return 'dog'
//   } with args:

const toUpper = s => String(s).toUpperCase()
// +t
test(toUpper)
toUpper.shallBe('DOG', 'dog')
toUpper.shallBe('UNDEFINED', undefined)
// -t

const square = n => n * n
test(square)
square.shallBe(16, 4)

const doubler = n => n * 2
test(doubler)
doubler.shallBe(20, 10)
doubler.shallBe(40, 20)

const compose = (f, g) => x => f(g(x))
// compose f, g => f(g())
// const squareDoubled = n => square(doubler(n))
const squareDoubled = compose(square, doubler)
test(squareDoubled)
squareDoubled.shallBe(4, 1)
squareDoubled.shallBe(16, 2)
squareDoubled.shallBe(25, 2.5)

const xaddOneToAll = xs => {
  let result = []
  for (let i = 0; i < xs.length; i++) {
    if (isOdd(xs[i])) {
      result.push(xs[i] + 1)
    } else {
      result.push(xs[i])
    }
  }
  return result
}
const map = (f, xs) => {
  let result = []
  for (let i = 0; i < xs.length; i++) {
    result.push(f(xs[i]))
  }
  return result
}

const addOneIfOdd = n => isOdd(n) ? n + 1 : n
const addOneToAll = xs => map(addOneIfOdd, xs)
test(addOneToAll)
addOneToAll.shallBe([2, 4, 4], [[2, 3, 4]])

suite()
