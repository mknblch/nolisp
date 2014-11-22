## Special Forms

### Conditions

+ cond
<pre>(cond (E1 B1) (E2 B2)	... )</pre>
iterates over arguments and treat each object as list `(E B)` if expression at `E` evaluates to true its body `B` is evaluated and the result returned. otherwise the next branch is taken. return null if no branch matches.

+ if
<pre>(if W T F)</pre>
if W evaluates to true, `T` is evaluated and its result returned otherwise `F` is evaluated. `F` is optional. if `W` evaluates to false and no `F` branch is given, `null` is returned.

### Basic:

	load
	quote
	setq
	let*
	[set, define]
	let
	fori

### Predicate:

	symbol?

### Lambda:

	function
	eval
	lambda
	defun
	funcall

### Macro:

	comma
	defmacro
	splice
	backquote

### Java:

	new
	class
	try
	call
	call-static
	instanceof?

## Forms

### Logic:

	and
	or
	xor
	not

### Basic:

	list
	progn

### Accessor:

	append
	cons
	[aget, array-get]
	[ainit, array-init]
	[aset, array-set]
	[amake, array-make]
	car
	nth
	cdr
	nthcdr

### Predicate:

	list?
	int?
	[eq?, equal?]
	null?
	real?
	string?
	lambda?
	macro?
	atom?

### Math:

	sin
	cos
	tan
	log
	log10
	[**, pow]
	ceil
	floor
	[+, add, sum]
	[-, sub]
	[*, mul]
	[/, div]
	[%, mod]

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
	[classof, classOf]

### Comparison:
these forms are used to compare numbers

+ `>`

	Greater

+ `>=`

	Greater Equal

+ `==`

	Equal

+ `!=`

	Not Equal

+ `<`

	Lower

+ `<=`

	Lower Equal
