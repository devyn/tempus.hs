# Syntax

A program is made up of definitions, separated by either semicolons
or newlines.

Definitions can either be expressions:

    y = a * b

Or functions:

    sec(angle) = 1 / cos(angle)

## Expressions

Expressions are first parsed for infix operators. Operator precedence
is according to the `infixes` constant in `AST.hs`.

If there are no infixes to parse, term rules apply:

1. Function application.
2. Prefix operator.
3. Value.

Function application is as defined:

    fun(a, b, c)

The only prefix operator right now is `-` (numeric negation).

## Values

1. An expression in parentheses.
2. A number.
3. A string.
4. A reference (to a defined variable).

Numbers are simple decimal floating point: `[0-9]\.[0-9]`.

Strings are enclosed in double quotation marks (") and the only escape sequence is `\"`, which represents a double quotation mark.

## Variables

Note that variable definitions are *not* static; they define the behavior of a reactive variable, the value of which may change when its inputs are changed. Even functions themselves are reactive and therefore potentially updateable.
