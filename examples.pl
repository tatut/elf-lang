:- module(examples, [ex/2, ex/3]).
:- use_module(elf_file).

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
ns do({($ > 10) if(\"big\") print}),

\"---\" print,

# then can be a value or a function
ns do({n:$, (n > 10) if({\"%d is big\" fmt(n) print})}),

\"---\" print,

# second arg to if can be specified for else
ns do({ ($ > 10) if(\"big\", $) print })",
   nil).

ex("FizzBuzz",
   "1 to(100) do({n| [15,3,5] map({(n % $) = 0}) cond(\"FizzBuzz\",\"Fizz\",\"Buzz\", $) print})", nil).

ex("FizzBuzz 2",
   "1 to(100) do({
 fizz: ($ % 3) = 0,
 buzz: ($ % 5) = 0,
 (fizz or buzz) if({ fizz if(\"Fizz\") ++ buzz if(\"Buzz\") }, $) print
 })",
   nil).

ex("Records",
   "# define Elf record
Elf{name, age},

# install a method on it
Elf.greet: { \"Hello %s, my name is %s and I'm %d years old!\" fmt($, my name, my age)},

# Make an instance
Elf{name: \"Elfo\", age: 666},

# increment age
_ age(_ age + 1),

# Call method on it
_ greet(\"world\")", `Hello world, my name is Elfo and I'm 667 years old!`).

%% ex("HTML generation",
%%    "# Define record for element
%% Elt{tag, attrs, content},
%% Text{text},

%% Text.html: {my text},

%% # render as html
%% Elt.html: {
%%  \"<%s\" fmt(my tag) ++
%%  my attrs mapcat({\" %s='%s'\" fmt($ first, $ last) }) ++
%%  \">\" ++
%%  my content mapcat(&html) ++
%%  \"</%s>\" fmt(my tag)
%% },

%% Elt.text: { my content(my content ++ [Text{text: $}]) },

%% # Define a dynamic method handler to add child
%% # this works by having elements as methods
%% Elt.dynamic: {method,args|
%%  n: Elt{tag: method},
%%  (args len >= 1) if({n attrs(args first)}),
%%  (args len = 2) if({args last call(n)}),
%%  my content(my content ++ [n])
%% },

%% # Use the HTML generation to output some markup

%% Todo{label, complete},
%% todos: [Todo{label:\"simple HTML generation\", complete: true},
%%         Todo{label:\"web framework?\", complete: false}],
%% h: Elt{tag: \"div\"},
%% _ ul([[\"class\",\"todos\"]],
%%       {ul| todos do({ li: ul li([[\"class\", $ complete if(\"complete\", \"active\")]]),
%%                       li input([[\"type\",\"checkbox\"]] ++ $ complete if([[\"checked\",\"1\"]])),
%%                       li text($ label) }) }),
%% h html",
%% % FIXME: should support self closing tags, but not important for demo
%%   `<div><ul class='todos'><li class='complete'><input type='checkbox' checked='1'></input>simple HTML generation</li><li class='active'><input type='checkbox'></input>web framework?</li></ul></div>`).

ex("Read file lines",
   "\"README.md\" lines first",
   `# Elf programming language`).

ex("Import code",
   "\"examples/elves.elf\" use,
youngest: elves minw(&age) _1,
youngest greet(\"world\")",
   `Hi world! My name is Biscuit Peppermint and I'm 75 years old!`).

ex("Advent of Code 2023, day 1", Code, [56506, 56017]) :-
   file_codes(`examples/aoc2023_day1.elf`, Cs),
   string_codes(Code, Cs).
