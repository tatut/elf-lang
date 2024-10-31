:- module(examples, [ex/2]).

ex("sum list of numbers",
   "[4, 2, 0, 6, 9] sum").

ex("reverse string",
   "\"elf helper\" reverse").

ex("format string",
   "\"%s day with %d%% chance of rain\" fmt(\"cloudy\",13)").

ex("FizzBuzz",
   "1 to(100) do(\\[(($ % 15) = 0), (($ % 3) = 0), (($ % 5) = 0)] cond(\"FizzBuzz\",\"Fizz\",\"Buzz\", $) print.)").

ex("Records",
   "# define Elf record
Elf{name, age},

# install a method on it
Elf.greet: \\ \"Hello %s, my names is %s!\" fmt($, my name).,

# Make an instance
Elf{name: \"Elfo\", age: 666},

# Call method on it
_ greet(\"world\")").

ex("foo", "bar").
