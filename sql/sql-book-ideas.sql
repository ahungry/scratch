-- We could also ensure we never run line 1 by the tool
PREPARE fooplan (int, text, bool, numeric, text) AS
  SELECT $1 as my_int
  , $2 as my_text
  , $3 as my_bool
  , $4 as my_numeric
  , $5::int as cast_int
  ;

-- We could easily have everything after some token in the file ignored by a QueryLoader
-- Essentially using it as some type of spec.
--explain analyze
EXECUTE fooplan(1, 'Hunter Valley', 't', 200.00, '42');
EXECUTE fooplan(5, 'Fake Valley', 'f', 400.04, '82');
