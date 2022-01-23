# Newlang

## Syntax

All statements must be followed by `;`.
Statements are assignments, expressions, returns, empty, or loops.

### Assignment

```js
x = 3;
```

### If

```js
x = if (y == null) 3 else 4;
x = if (y == null) {
    print("y is null");
    3;
} else {
    print("y is not null");
    4;
};
x = if (y == null) { y = init(); y; }
```

### For

```js
for (i = 0; i != 5; i = i + 1) {
    print(i);
}
```

## Types

### Null

Literal: `null`.

### Integer

Literal: `123` (decimal) / `0xbeef` (hex)

### Float

Literal: `0.123`

### String

Literal: `"abc\ndef\tgh"`

### Function

```text
function (a, b, c) { return a + b + c; }
```

## Todo

### List

```js
[1, 2, "hi", 4, 5]
```

Get an element using the `[]` operator.

### Operator

Support `<`, `>`, `<=`, `>=`, `&`, `|`, `[]`.

| Operator                         | Call fun if on objects                   |
| -------------------------------- | ---------------------------------------- |
| `<`, `>`, `<=`, `>=`, `==`, `!=` | `Class.__cmp(x, y)`                      |
| `&&`                             | `Class.__land(x, y)`                     |
| `||`                             | `Class.__lor(x, y)`                      |
| `!`                              | `Class.__lnot(x)`                        |
| `+`                              | `Class.__plus(x, y)`, `Class.__uplus(x)` |
| `&`                              | `Class.__band(x, y)`                     |
| `[]`, `[]=`                      | `Class.__get(k)` , `Class.__set(k, v)`   |

### Class

```text
class C {
    x = 5;
    f = function(this) { this.y = 6; };
};
```

Classes are also objects. Usage: `C["x"]` or `C.x` (equivalent).

The above class is equivalent to the object

```js
{
    "__type": "class",
    "__classname": "C",
    "x": 5,
    "f": function (this) { this.y = 6; }
}
```

#### Instantialization

Use builtin function `new(class)`.

```js
c = new(C);
```

`new` will call the `__new(this)` function defined in the class object.
`new(class)` basically returns an object of the form

```js
{
    "__type": class.__classname
}
```

#### Inheritance

Only supports single inheritance. The inherited class is defined in the class object
with the `__superclass` field.

```js
{
    "__type": "class",
    "__classname": "Dog",
    "__superclass": "Animal"
}
```

### Hashtable, a.k.a. object

```js
h = {"x": 5, "y": 6};
o = {
    "__type": "AClass",
    "instancevar": 123
};
```

If a hashtable is used as a hashtable, it's just like above.

If a hashtable is used as an object, it has to have a `__type` field.
If it is used as an class object, it has to have a `__classname` field.
Any lookup on an object will first lookup through the inheritance stack
(using `__type`).
