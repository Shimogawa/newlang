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
