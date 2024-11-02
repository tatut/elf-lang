:- module(examples, [ex/2]).

ex("sum list of numbers",
   "[4, 2, 0, 6, 9] sum").

ex("reverse string",
   "\"elf helper\" reverse").

ex("string digits",
   "\"some4digits in this string 2!\" keep(&digit), (_ first * 10) + _ last").

ex("format string",
   "\"%s day with %d%% chance of rain\" fmt(\"cloudy\",13)").

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
ns do(\\ ($ > 10) if(\"big\", $) print.)").

ex("FizzBuzz",
   "1 to(100) do(\\ n:$, [15,3,5] map(\\ (n % $) = 0.) cond(\"FizzBuzz\",\"Fizz\",\"Buzz\", $) print.)").

ex("Records",
   "# define Elf record
Elf{name, age},

# install a method on it
Elf.greet: \\ \"Hello %s, my names is %s! and I'm %d years old\" fmt($, my name, my age).,

# Make an instance
Elf{name: \"Elfo\", age: 666},

# increment age
_ age(_ age + 1),

# Call method on it
_ greet(\"world\")").
