# Elf programming language

![test workflow](https://github.com/tatut/elf-lang/actions/workflows/test.yml/badge.svg)

![elf-lang logo](https://github.com/tatut/elf-lang/blob/main/elf-lang-logo-small.png?raw=true)

This is the Elf programming language.

Designed to get help those elves get their xmas duties done
without tripping into lava.

Try the playground at: https://tatut.github.io/elf-lang/

## Syntax and evaluation

Syntax is a little bit Smalltalkish, with Perlish implicit values and Clojurish
functional programming with records & methods.... all implemented lovingly in Prolog.

Like Smalltalk all method calls are evaluated first left to right, then all binary operators left to right.
There is no operator precedence, use parenthesis to group math expressions.

```
# wrong appends 5 (length of string "World") to "Hello "
"Hello " ++ "World" len

# right, calculates length of "Hello World" (11)
("Hello " ++ "World") len
```

**Why?**

Why not.


### Syntax

**Syntax overview**

Comments start with `#` and end in newline.

```
"I'm an Elf"  # a string obviously (actually a list of ascii code ints)

@?            # the ´?´ character (as ascii integer 62)

420.69        # a decimal number

666           # an integer

[4, 6, 9]       # list of integers

true          # the true value
false         # the false value
nil           # the nothing value

$             # reference to the 1st argument in function
$1 ... $9     # reference to the Nth argument in function
my            # reference current object in methods
&foo          # reference to method named foo

_             # reference to the value of the last statement

{ $ * 2 }     # function of arity 1

{a,b| a + b}  # function of arity 2 with named arguments


["hello", "there"]  # list

Elf{name,age} # object record definition
Elf.greet: { "Hello %s, my name is %s." fmt(my name, $) } # define method on record

elf greet("world") # call method

Elf{name: "Jolly Twinkletoes", age: 1607} # an object

Elf new("Scrappy Fairytoes", 666) # programmatic construction

%{"foo": 42} # associative key/value map

the_answer: 42 # assignment statement

"naughty_list.txt" lines # call method lines on string

# if is just a method taking 1 or 2 args (then,else)
# if the value is a function it is called with the value
# otherwise it is returned
somebool if(thenblock)
somebool ife(thenblock, elseblock)

```

### Valid names

Names of any methods or assignments must begin with `_` or a letter (case doesn't matter),
after that it may contain alphanumeric and `-`, `_` and `?` characters.

For example these are valid names:
- `my_variable`
- `is-valid?`
- `Foo1`

Reserved words (`and`, `or` and `_`) cannot be used as names.

### Binary operations

Elf supports the following binary operators:
- `+`,`-`,`*`,`/`,`%` numeric addition, substraction, multiplication, division and modulo (number)
- `<`, `<=`, `>`, `>=` numeric less than, less than or equals, greater than and greater than or equals (true/false)
- `=` value equality (true/false)
- `++` list append (yields list that is the appended left and right hand side lists)
- `and` boolean truth (true if both left and right hand sides are truthy, false otherwise)
- `or` boolean or (true if either left or right hand side is truthy, false otherwise)

# Records

Elf supports records which have a predefined list of fields.
Records can be instantiated by `RecordName{field1Name: field1Val, ...}` any fields not given a value will have the initial value `nil`.

Record fields are automatically added as method to get/set the value.
The 0-arity method gets the value and 1-arity sets it.

Records also support user defined methods that are functions with the
special name `my` referring to the record instance.

There is a special method called `dynamic` that will be invoked for any
methods not defined. This can be used for meta-programming.
The method gets the name of the undefined method as string and a list
of its arguments.
