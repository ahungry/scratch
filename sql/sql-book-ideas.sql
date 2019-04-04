-- See if we can make a polyglot SQL
PREPARE fooplan (int, text, bool, numeric, text) AS
  SELECT $1 as my_int
  , $2 as my_text
  , $3 as my_bool
  , $4 as my_numeric
  , $5::int as cast_int
  ;
--explain analyze
EXECUTE fooplan(1, 'Hunter Valley', 't', 200.00, '42');
