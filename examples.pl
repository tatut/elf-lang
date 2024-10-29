:- module(examples, [ex/2]).

ex("sum list of numbers",
   "[4 2 0 6 9] sum").

ex("reverse string",
   "\"elf helper\" reverse").

ex("format string",
   "\"%s day with %d%% chance of rain\" fmt(\"cloudy\" 13)").

ex("foo", "bar").
