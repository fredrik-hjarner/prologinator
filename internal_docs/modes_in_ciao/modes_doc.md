# Classical Prolog modes

**Author:** Manuel Hermenegildo

## Module

This package defines a number of **modes** which are frequently useful in programs when describing predicates, e.g., via `pred` assertions (or *doccomments*, see below). They correspond to the modes used in many classical Prolog texts and code as documentation, with some additions. In Ciao these modes are actually syntactic sugar for assertions, so that they can be checked either statically or dynamically. Note that some of these modes use the same symbol as one of the `basicmodes` and `isomodes` packages (see "Some basic Prolog modes" and "ISO-Prolog modes") but have in some cases subtly different meaning.

As an example, the following declaration:

```prolog
:- pred is(-,+).
```

Expresses that `is/2` should be called with the second argument bound and it will bind the first argument. Also:

```prolog
:- pred is(-num,+arithexpression).
```

(more precise than the above), expresses that `is/2` should be called with the second argument *instantiated* to an arithmetic expression and that on success it will bind the first argument to a number. The argument of a mode as above can be any property, including, e.g., regular types.

The first declaration is equivalent to (and is in fact translated to) the assertion:

```prolog
:- pred is(X,Y) : nonvar(Y) => nonvar(X).
```

and the second one to:

```prolog
:- pred is(X,Y) : arithexpression(Y) => num(X).
```

Modes can also be included inside comments in markdown format (see the `doccomments` and `markdown` libraries). For example:

```prolog
%! pred is(-,+):
%  Evaluates the expression in the second argument
%  and binds the first argument with the result.
```

which are also translated to the corresponding assertions.

## Mode Definitions

### `+/1`

The argument should be bound (nonvar) when the predicate is called. For example:

```prolog
:- pred + > +.
```

expresses that both arguments of `>/2` should be bound when the predicate is called.

### `+/2`

This argument should be *instantiated* when the predicate is called. For example:

```prolog
:- pred +arithexpression > +arithexpression.
```

expresses that both arguments of `>/2` should be bound to arithmetic expressions (have the `arithexpression` property) when the predicate is called.

### `-/1`

The argument is an output argument. It may be bound or not at call time. It will be bound (nonvar) if the predicate succeeds.

### `-/2`

The argument is an output argument. It may be bound or not at call time. It will be *instantiated* to a term that has the indicated type or property if the predicate succeeds. For example, this assertion:

```prolog
:- pred length(-list,-int).
```

expresses that `length/2` can be called in any mode, but on output the second argument will be *instantiated* to a number and the first one will be *instantiated* to a list. Note that this does not mean that the list will be ground, but rather that it will be a complete list but whose elements can be any term, including variables (see the discussion of instantitation and compatibility types in "Declaring regular types").

### `--/1`

The argument should be a free variable (i.e., unbound) when the predicate is called.

### `--/2`

The argument should be a free variable (i.e., unbound) when the predicate is called and will be bound to a term that has the indicated type or property in general, if the predicate succeeds.

### `?/1`

No information is given on this argument.

### `?/2`

The argument can be a variable or, if it is instantiated, it is to a term that is *compatible* with the indicated type or property.

### `@/1`

The argument will not be further instantiated, i.e., will not be more instantiated than when the predicate is called.

### `@/2`

The argument will not be further instantiated, i.e., will not be more instantiated than when the predicate is called, and the term is *compatible* with the indicated type or property.

### `in/1`

The argument is ground at call time.

### `in/2`

The argument is ground at call time and is *compatible* with the indicated type or property.

### `++/1`

Same as `in`: the argument is ground at call time.

### `++/2`

Same as `in`: the argument is ground at call time and is *compatible* with the indicated type or property.

### `out/1`

The argument is a variable when the predicate is called and will be ground if the predicate succeeds.

### `out/2`

The argument is a variable when the predicate is called and will be bound to a ground term that is *compatible* with the indicated type or property, if the predicate succeeds.

### `go/1`

The argument is ground by the predicate, if it succeeds.

### `go/2`

The argument is ground by the predicate to a ground term that is *compatible* with the indicated type or property, if the predicate succeeds.
