# Grimoire-Lang
A OOP programming language

# Variables and Comments
```
let [id]: [type] = [expression]; // new var, this is a comment
```
- immutable by default, can use `mut` to make it mutable
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
def [name] -> [return type] (var1: [param type], var2: [param type]):
	// function body
end

// lambads

// function types:
([param type], ...) -> [return type]
```
# Classes
```
class [name] extends [parent class]

// methods are the same as functions,
// but must take self as first param
// and can be static and/or public or private

```
# Arrays
```

```
