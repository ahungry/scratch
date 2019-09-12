// This would be hidden implementation code.
// Define a couple types
const Num = Symbol('n')
const Str = Symbol('s')

// Holds a function with typing info included
const fn = (i, o, f) => ({ i, o, f })

// Call the function (unary arity atm, although this could be dynamic)
const invoke = ({ f }) => (x) => f(x)
const in_type = ({ i }) => i
const out_type = ({ o }) => o

const err = (_i, _o) => { throw new Error('Incompatible types!') }
const assert_types = (f, g) => in_type(f) === out_type(g) ? true : err(in_type(f), out_type(g))
const comp = (f, g) => assert_types(f, g) ?
  fn(out_type(g), in_type(f), (x) => invoke(f)(invoke(g)(x))) :
  false

const compmap = (f, g) => assert_types(f, g) ?
  fn(out_type(g), in_type(g), x => invoke(g)(x).map(y => invoke(f)(y))) :
  false

// This would be the user space code.

// Make an instance of a 'typed' function
const add_one = fn(Num, Num, n => n + 1)
const stringer = fn(Num, Str, n => "Your number is: " + String(n))

console.log(invoke(add_one)(3)) // 4
console.log(invoke(stringer)(3)) // "Your number is: 3"

const add_two = comp(add_one, add_one)
console.log(invoke(add_two)(10)) // evals to 12

// This will throw an error about invalid composition, as we can't add one to a string
// This could be more lenient and just print a warning etc.
// const add_bad = comp(add_one, stringer)

const range = fn(Num, Num, (n) => [...Array(n).keys()])
const add_two_to_list = compmap(add_two, range)
console.log(invoke(add_two_to_list)(3)) // produces [2, 3, 4]

// This will throw and be an incompatible combo
const get_months = fn(Str, Str, _ => ["Jan", "Feb", "Mar"])
// const add_two_to_names = compmap(add_two, get_months)
