# NOLISP ( not only lisp )

Learning functional programming the hard way - by implementing an interpreter from scratch.

This language is for fun and didactic purpose only and hardly usable in productive code. Everything is hardcoded without dependencies on 3rd-party libraries.

## Usage

Take a look into the integration tests ;)

The app module generates a runnable jar to evaluate files, run single commands or open a simple command shell.

Use `-e` switch to toggle evaluation mode.

    java -Xss32M -jar NOLISP.jar -e "(* (* 2 3) 7)"

Use `-f` switch to evaluate a file.

    java -Xss32M -jar NOLISP.jar -f ./scripts/someFile.nl

Use `-l` to enter repl-mode.

    java -Xss32M -jar NOLISP.jar -l

*Note:* Nolisp relies heavily on recursion therefore a big execution stack is needed (use -Xss VM argument).

## Language characteristics

Basic Lisp syntax is supported including syntactic sugar like quote ( ' ), backquote ( \` ), comma ( , ), sharp-quote ( \#' ) and the splicing of lists at parse-time (with at or dot syntax).

NOLISP bind its functions and variables in the same context. This makes #' optional but permits variables and functions with identical symbol-name.

The Lexer can read basic types like list, array, integer (standard or hexadecimal notation), real and null as well as more complex types like escaped string, one line comment, long, BigInteger and BigDecimal.

Syntactic sugar (e.g. quote, backquote, ..) is processed in the Parser. Upon occurrence of a special token the Parser transforms these tokens into ListStructs containing a special symbol in their car-part and the following element in their cdr-part.

Implementing new functionality is done by defining static functions with a \@Define annotation. These are wrapped into Forms and bound to the Context. SpecialForms can be made by using an additional \@Special annotation.

## Functionality

The current set of functions work more or less like Common Lisp - without guarantee. Take a look into the integration tests to see how they (should) work.

## 26 Special Forms

### Condition:
	cond
	if

### Basic:
	setq
	let
	let*
	[set, define]
	[load, load-file]
	load-url
	fori
	quote ; created by the parser when reading a quote-sign ( ' )

### Predicate:
	symbol?

### Lambda:
	lambda
	eval
	defun
	funcall
	function ; created by the parser when reading a sharp-sign ( # )

### Macro:
	defmacro
	comma ; created by the parser when reading a comma-sign ( , )
	splice ; created by the parser when reading an at-sign ( @ )
	backquote ; created by the parser when reading a backquote-sign ( ` )

### Java:
	new
	class
	call
	call-static
	try
	instanceof?

## 55 Forms

### Logic:
	and
	or
	xor
	not

### Basic:
	list
	[llength, list-length]
	progn

### Accessor:
	car
	cdr
	cons
	append
	nth
	nthcdr
	[aget, array-get]
	[ainit, array-init]
	[aset, array-set]
	[amake, array-make]

### Predicate:
	lambda?
	macro?
	atom?
	null?
	int?
	real?
	string?
	list?
	[eq?, equal?]

### Math:
	[+, add, sum]
	[-, sub]
	[*, mul]
	[/, div]
	[%, mod]
	[**, pow]
	sin
	cos
	tan
	log
	log10
	ceil
	floor
	[rint, random-int]

### Lambda:
	lbody
	largs

### Console:
	print
	printf
	pprint
	sprint

### Java:
	throw
	utime
	[classof, classOf]

### Comparison:
	==
	!=
	>
	>=
	<
	<=

## Current Flaws

+ The interpreter easily reaches recursion limit when using recursive lambdas (even with tail-recursion) - maybe a stack based approach could help.
+ The implementation of builtin-functions is easy for the coder but not optimal for the interpreter which wraps a static function in an executable form using reflection - a Strategy-pattern may help.
+ Dynamic list-splicing is not supported (the expression `(setq a '(6 7)) (* @a)` wont work).

