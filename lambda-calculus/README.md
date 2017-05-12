# Lambda Calculus

The goal here is to write something that can parse and reduce expressions in the (untyped) lambda calculus, and possibly an esoteric programming language providing a very thin layer on top of it.

## Syntax

We operate on expressions. An expression has one of the following forms:

### Literal

`$a`

A variable. We require all literals to be prefixed with a $ sign, because the parser isn't that smart and wouldn't know if "hello" were one variable or three or five otherwise.

### Binding

`\$a.$b`

We say that the variable coming after the backslash is *bound*. Semantically, think of this like constructing a function that takes a variable.

### Application

`e f`

Simple concatenation of two expressions.

## Implementation

I'm writing it in Python, because it's the language I know best that has first-class functions and isn't JavaScript or compile-to-JavaScript.

## Parser

## Evaluation Strategy
