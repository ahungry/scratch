#! /usr/bin/gforth

require random.fs

utime drop seed !

: r random 1 + ;

CREATE x 10 r r ,
variable c

: guess
  1 c +!
  ." Guess a number between 1 and 10" CR
  pad 40 accept
  pad swap s>number?
  >r d>s r> drop
  dup x @ > if
    ." Too high!" CR recurse
  endif
  dup x @ < if
    ." Too low!" CR recurse
  endif
  dup dup ;

guess
CR ." You win!" CR
." It took you " c ? ." guess(es)!" CR
bye
