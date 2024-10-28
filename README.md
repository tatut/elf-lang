# Elf Helper programming language

This is the Elf Helper (eh) programming language.
Designed to get help those elves get their xmas duties done
without tripping into lava.


## Syntax

Syntax is a little bit Smalltalkish, with Perlish implicit values and Clojurish
functional programming with old style JavaScriptish prototypal inheritance.... all implemented lovingly in Prolog.

**Why?**

Why not.


### Syntax

**Syntax overview**

Comments start with `#` and end in newline.

```
"I'm an Elf"  # a string obviously

@?            # the ´?´ character (as ascii integer 62)

420.69        # a decimal number

666           # an integer

true          # the true value
false         # the false value
nil           # the nothing value

$             # reference to the 1st argument in function
$1 ... $9     # reference to the Nth argument in function
me            # reference current object in methods
&foo          # reference to method named foo

_             # reference to the value of the last statement

\ $ * 2.      # function of arity 1

["hello" "there"]  # list

{name: "Jolly Twinkletoes", age: 1607} # an object

42 -> the_answer # assignment statement

"naughty_list.txt" lines # call method lines on string


```


chars:
@?  (the question mark char)


dictionaries:
{name: "Elf" age: 1024}

boolean
true/false

if/else:

somebool if(thenblock)
somebool ife(thenblock elseblock)

assign:

42 -> foo

fn def:

\ $ * 2. -> double

comments:

# comment to end of line


inline list:

the , operator joins items into list:

42, 666  will yield [42, 666] list

more comma calls will add to the end
