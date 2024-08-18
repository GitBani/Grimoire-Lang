# Grimoire-Lang
A OOP programming language

# Variables and Comments
```
let [id]: [type] = [expression]; // new var, this is a comment
```
- immutable by default, can use `mut` to make it mutable
- not nullable by default, can use `?` to make it nullable

# Types
- int
- float
- bool
- char
- string
- array (dynamic)
let arr: int[] = [12,3,4,5] 

# Operators
- arithmetic operators
- `and`, `or`, `not`
- equality & inequality operators
- unary negation (numbers / bools)
- bitwise operations
- grouping ()

# Control Flow
I might want to make these expressions.
```
if [condition]:
	...
else if:
	...
else:
	...
end
```
```
for [var] in [Range or iterable]:
	...
end
while [condition]:
	...
end
```
# Functions
```
def [name] ([var]: [type], ...) -> [return type]:
	// function body
end

// lambdas
\([var]: [type], ...) -> [return type]: ... end

// function types:
([param type], ...) -> [return type]
```
- Can be higher-order
# Classes
```
class [name] inherits [parent class] implements [interface names]

// methods are the same as functions,
// but must take self as first param
// and can be static and/or public or private

```

# Cool other stuff
- pipe operator
- pattern matching
- ADT like enums
- generics (TS like type-system)
- null coalescing operator (??)
- type inference

# Temp
- print as statement until we have stdio