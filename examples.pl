:- module(examples, [ex/2, ex/3]).

ex(Name, Code) :- ex(Name, Code, _Result).

ex("sum list of numbers",
   "[4, 2, 0, 6, 9] sum",
   21).

ex("reverse string",
   "\"elf helper\" reverse",
  `repleh fle`).

ex("string digits",
   "\"some4digits in this string 2!\" keep(&digit), (_ first * 10) + _ last",
  42).

ex("format string",
   "\"%s day with %d%% chance of rain\" fmt(\"cloudy\",13)",
   `cloudy day with 13% chance of rain`).

ex("if/else",
   "ns: [12,5,42,1],

# if without else returns nil
# nil prints nothing
ns do(\\ ($ > 10) if(\"big\") print.),

\"---\" print,

# then can be a value or a function
ns do(\\ n:$, (n > 10) if(\\ \"%d is big\" fmt(n) print.).),

\"---\" print,

# second arg to if can be specified for else
ns do(\\ ($ > 10) if(\"big\", $) print.)",
   nil).

ex("FizzBuzz",
   "1 to(100) do(\\ n:$, [15,3,5] map(\\ (n % $) = 0.) cond(\"FizzBuzz\",\"Fizz\",\"Buzz\", $) print.)", nil).

ex("FizzBuzz 2",
   "1 to(100) do(\\
 fizz: ($ % 3) = 0,
 buzz: ($ % 5) = 0,
 (fizz or buzz) if(\\ (fizz if(\"Fizz\") ++ buzz if(\"Buzz\"))., $) print.)",
   nil).

ex("Records",
   "# define Elf record
Elf{name, age},

# install a method on it
Elf.greet: \\ \"Hello %s, my name is %s and I'm %d years old!\" fmt($, my name, my age).,

# Make an instance
Elf{name: \"Elfo\", age: 666},

# increment age
_ age(_ age + 1),

# Call method on it
_ greet(\"world\")", `Hello world, my name is Elfo and I'm 667 years old!`).
