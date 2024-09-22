# Grimoire-Lang
A OOP+Functional programming language

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
- array
let arr: int[] = {12,3,4,5}

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
if [condition] {
	...
} else if [condition] {
	...
} else {
	...
}
```
```
for [var] in [Range or iterable] {
	...
}
while [condition] {
	...
}
```
# Functions
```
def [name] ([var]: [type], ...): [return type] {
	// function body
}

// lambdas
([var]: [type], ...): [type] -> [expression or block] 

// function types:
([param type], ...) -> [return type]
```
- Can be higher-order
# Classes
```
class [name] implements [interface names] {}
// can't extend concrete types
// interfaces can have default implementations

// methods are the same as functions,
// but must take self as first param
// and can be static and/or public or private (private by default)

interface [name] {
	// method signatures / default implementations
}

```

# Cool other stuff
- type inference
- pipe operator
- ADT like enums
- pattern matching
- generics (TS like type-system)
- think of how to handle nulls, or to exclude them entirely
- null coalescing operator (??)

# Temp
- print as statement until we have stdio