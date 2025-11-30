# The Ciao assertion language

**Authors:** Manuel Hermenegildo, Francisco Bueno, German Puebla

**Copyright:** © 1989-2002 The CLIP Group / UPM

## Summary

This library provides packages and modules which allow including **program assertions** in user programs. Such assertions can be used to describe predicates, properties, modules, applications, etc. These descriptions can contain formal specifications (such as sets of preconditions, post-conditions, or descriptions of computations) as well as machine-readable textual comments. The information contained in the assertions will be used as input by other tools for tasks such as static or dynamic debugging, verification, automatic documentation generation, etc.

## Usage

```prolog
:- use_package(assertions).
```

or

```prolog
:- module(...,...,[assertions]).
```

Use the `assertions_basic` package to disable the implicit use of the `basic_props` module.

## Module

The `assertions` package adds a number of new declaration definitions and new operator definitions which allow including **program assertions** in user programs. Such assertions can be used to describe predicates, properties, modules, applications, etc. These descriptions can contain formal specifications (such as sets of preconditions, post-conditions, or descriptions of computations) as well as machine-readable textual comments.

This module is part of the `assertions` library. It defines the basic code-related assertions, i.e., those intended to be used mainly by compilation-related tools, such as the static analyzer or the run-time test generator.

Here we document mainly the use of assertions for providing **specifications** for predicates and other program elements, as well as the locations within assertions where machine-readable documentation strings can be placed. The commands that can be used in the documentation strings and other directives that can be used to provide additional machine-readable comments are described in the autodocumenter (`lpdoc`) manual.

There are two kinds of assertions: predicate assertions and program point assertions. Predicate assertions are placed as directives in the source code, i.e., preceded by `:-`. Program point assertions are placed as literals in clause bodies. Additional documentation on the syntax and fields of predicate assertions can be found in the "Types and properties related to assertions" module.

### Getting more information

This documentation is intended to provide information at a "reference manual" level. For more tutorial introductions to the assertion language and more examples please see the cited papers and the `ciaopp` tutorial.

The assertion language as implemented in this library essentially follows these documents, although, due to its evolution, it may differ in some details. The purpose of this manual is to document precisely what the implementation of the library supports at any given point in time.

### Some attention points

- **Formatting commands within text strings:** Many of the predicates defined in these modules include arguments intended for providing textual information. This includes titles, descriptions, comments, etc. The type of this argument is a character string. In order for the automatic generation of documentation to work correctly, this **character string** should adhere to certain conventions. See the description of the `docstring/1` type/grammar for details.

- **Referring to variables:** In order for the automatic documentation system to work correctly, **variable names** (for example, when referring to arguments in the head patterns of *pred* declarations) must be surrounded by a `@var` command. For example, `@var{VariableName}` should be used for referring to the variable "VariableName", which will appear then formatted as follows: *VariableName*. See the description of the `docstring/1` type/grammar for details.

---

## Predicate-level assertions

### `pred/1`

**pred assertion** - `pred` assertions are the most general type of assertion, and they are used to provide information on the set of admissible calls to a predicate, the set of successes, global properties, and documentation. The body of a `pred` assertion (its only argument) contains properties or comments in different fields following the formats specified by `assrt_body/1`.

There can be more than one of these assertions per predicate, in which case each one represents a possible **mode** of use (**usage**) of the predicate. The exact scope of the usage is defined by the properties given for calls in the body of each assertion (which should thus distinguish the different usages intended). Predicates are typically specified using a *set* of `pred` assertions, so that together they cover all ways in which the predicate is intended be used (all usages). Each `pred` assertion is translated internally to a `calls` assertion covering all the calling modes and a `success` assertion covering the successes for those calls.

For example, the following assertions would describe all intended modes (and the only modes) of use of a predicate `length/2`:

```prolog
:- pred length(L,N) : list * var => list * integer
    # "Computes the length of L.".
:- pred length(L,N) : var * integer => list * integer
    # "Outputs L of length N.".
:- pred length(L,N) : list * integer => list * integer
    # "Checks that L is of length N.".
```

### `pred/2`

**pred assertion** - A `pred` assertion (see `pred/1`) can be preceded (as most other assertions) by an assertion status (`assrt_status/1`). If no assertion status is present (i.e., in `pred/1` assertions) the status is assumed to be `check`.

For example, the following assertion:

```prolog
:- pred length(L,N) : list * var => list * integer.
```

is equivalent to:

```prolog
:- check pred length(L,N) : list * var => list * integer.
```

---

### `calls/1`

**calls assertion** - `calls` assertions are similar to `pred/1` assertions but they only provide information about the calls to a predicate. The set of calls assertions for a predicate describe *all* possible calls to the predicate.

For example, the following assertion describes all possible calls to predicate `is/2`:

```prolog
:- calls is(term,arithexpression).
```

### `calls/2`

**calls assertion** - A `calls` assertion can be preceded (as most other assertions) by an assertion status (`assrt_status/1`). If no assertion status is present (i.e., in `calls/1` assertions) the status is assumed to be `check`.

---

### `success/1`

**success assertion** - `success` assertions specify properties of the answers of a predicate, similarly to the corresponding `success` field of a `pred` assertion. The assertion can be limited to apply to a particular way of calling the predicate if a `calls` field is present. However, unlike `pred` or `calls` assertions, the predicate is not forced to be called only that way.

For example, the following assertion specifies the answers of the `length/2` predicate *if* it is called as in the first mode of usage above (note that the previous pred assertion already conveys such information, however it also restricts the set of admissible `calls`, while the success assertion does not):

```prolog
:- success length(L,N) : list * var => list * integer.
```

### `success/2`

**success assertion** - A `success` assertion can be preceded (as most other assertions) by an assertion status (`assrt_status/1`). If no assertion status is present (i.e., in `success/1` assertions) the status is assumed to be `check`.

---

### `comp/1`

**comp assertion** - `comp` assertions specify global properties of the execution of a predicate, similarly to the corresponding `comp` field of a `pred` assertion. The assertion can be limited to apply to a particular way of calling the predicate if a `calls` field is present. However, unlike `pred` or `calls` assertions, the predicate is not forced to be called only that way.

For example, the following assertion specifies that the computation of `append/3` will not fail *if* it is called as described (but does not force the predicate to be called only that way):

```prolog
:- comp append(Xs,Ys,Zs) : var * var * var + not_fail.
```

### `comp/2`

**comp assertion** - A `comp` assertion can be preceded (as most other assertions) by an assertion status (`assrt_status/1`). If no assertion status is present (i.e., in `comp/1` assertions) the status is assumed to be `check`.

---

### `prop/1`

**prop assertion** - `prop` assertions are similar to a `pred/1` assertions but they flag that the predicate being documented is also a **property**.

Properties are standard predicates, but which are *guaranteed to terminate for any possible instantiation state of their argument(s)*, do not perform side-effects which may interfere with the program behaviour, and do not further instantiate their arguments or add new constraints.

Provided the above holds, properties can thus be safely used as **run-time checks**. The program transformation used in `ciaopp` for run-time checking guarantees the third requirement. It also performs some basic checks on properties which in most cases are enough for the second requirement. However, it is the user's responsibility to guarantee termination of the properties defined. (See also "Declaring regular types" for some considerations applicable to writing properties.)

The set of properties is thus a strict subset of the set of predicates. Note that properties, in addition to being used to describe characteristics of arguments in assertions, they can also be executed (called) as any other predicates.

### `prop/2`

**prop assertion** - This assertion is similar to a `prop/1` assertion but it is explicitly qualified. Non-qualified `prop/1` assertions are assumed the qualifier `check`.

---

## Assertions for testing

### `test/1`

**test assertion** - This assertion is similar to a success assertion but it specifies a concrete test case to be run in order verify (partially) that the predicate is working as expected. For example, the following test will verify that the length predicate works well for the particular list given:

```prolog
:- test length(L,N) : ( L = [1,2,5,2] ) => ( N = 4 ).
```

### `test/2`

**test assertion** - This assertion is similar to a `test/1` assertion but it is explicitly qualified with an **assertion status**. Non-qualified `test/1` assertions are assumed to have `check` status. In this context, check means that the test should be executed when the developer runs the test battery.

### `texec/1`

**texec assertion** - This assertion is similar to a `calls/1` assertion but it is used to provide input data and execution commands for run-time testing.

### `texec/2`

**texec assertion** - This assertion is similar to a `texec/1` assertion but it is explicitly qualified with an **assertion status**. Non-qualified `texec/1` assertions are assumed to have `check` status.

---

## Module-level assertions

### `entry/1`

**entry assertion** - `entry` assertions provide information about the *external* calls to a predicate. They are identical syntactically to a `calls/1` assertion. However, they describe only the external calls, i.e., the calls to the exported predicates of a module from outside the module, or calls to the predicates in a non-modular file from other files (or the user).

These assertions are *trusted* by the compiler, i.e., they are similar to writing a `trust calls` assertion (except for referring only to the external calls). As a result, if they are erroneous they can introduce bugs in programs. Thus, `entry` assertions should be written with care.

An important use of these assertions is in **providing information to the compiler** which it may not be able to infer from the program. The main use is in providing information on the ways in which exported predicates of a module will be called from outside the module. This will greatly improve the precision of the analyzer, which otherwise has to assume that the arguments that exported predicates receive are any arbitrary term.

The distinction between external and internal calls is not always relevant and in those cases the use of `trust calls` assertions is preferred. Because of this, `entry` assertions may be deprecated in the future, since the distinction between external and internal calls can also be achieved by means of a bridge predicate.

---

### `exit/1`

**exit assertion** - This type of assertion provides information about the answers that an (exported) predicate provides for *external* calls. It is identical syntactically to a `success/1` assertion. However, it describes only external answers, i.e., answers to the exported predicates of a module from outside the module, or answers to the predicates in a non-modular file from other files (or the user). The described answers may be conditioned to a particular way of calling the predicate. E.g.:

```prolog
:- exit length(L,N) : list * var => list * integer.
```

These assertions are *trusted* by the compiler, i.e., they are similar to writing a `trust success` assertion (except for referring only to the external calls). As a result, if they are erroneous they can introduce bugs in programs. Thus, `exit` assertions should be written with care.

The distinction between external and internal calls is not always relevant and in those cases the use of `trust success` assertions is preferred. Because of this, `entry` assertions may be deprecated in the future, since the distinction between external and internal calls can also be achieved by means of a bridge predicate.

### `exit/2`

**exit assertion** - This assertion is similar to an `exit/1` assertion but it is explicitly qualified with an **assertion status**. Non-qualified `exit/1` assertions are assumed the qualifier `check`.

---

## Assertion macros

### `modedef/1`

This assertion is used to define modes. A mode defines in a compact way a set of call and success properties. Once defined, modes can be applied to predicate arguments in assertions. The meaning of this application is that the call and success properties defined by the mode hold for the argument to which the mode is applied. Thus, a mode is conceptually a "property macro."

The syntax of mode definitions is similar to that of `pred` declarations. For example, the following set of assertions:

```prolog
:- modedef +A : nonvar(A) # "A is bound upon predicate entry.".

:- pred p(+A,B) : integer(A) =>  ground(B).
```

is equivalent to:

```prolog
:- pred p(A,B) : (nonvar(A),integer(A)) =>  ground(B)
   # "A is bound upon predicate entry.".
```

---

### `decl/1`

**decl assertion** - This assertion is similar to a `pred/1` assertion but it is used to describe declarations instead of predicates.

### `decl/2`

**decl assertion** - This assertion is similar to a `decl/1` assertion but it also has a status. Non-qualified `decl/1` assertions are assumed to have status `check`.

### `doc/2`

Documentation **comment assertion**. This assertion provides a text `Comment` for a given predicate `Pred`, as well as other directives for the documenter.

### `comment/2`

An alias for `doc/2` (deprecated, for compatibility with older versions).

---

## Program-point assertions

### `check/1`

**check assertion** - This assertion provides information on a clause program point (position in the body of a clause). Calls to a `check/1` assertion can appear in the body of a clause in any place where a literal can normally appear. The property defined by `PropertyConjunction` should hold in all the run-time stores corresponding to that program point. See also "Run-time checking of assertions."

### `trust/1`

**trust assertion** - This assertion also provides information on a clause program point. It is identical syntactically to a `check/1` assertion. However, the properties stated are not taken as something to be checked but are instead *trusted* by the compiler. While the compiler may in some cases detect an inconsistency between a `trust/1` assertion and the program, in all other cases the information given in the assertion will be taken to be true. As a result, if these assertions are erroneous they can introduce bugs in programs. Thus, `trust/1` assertions should be written with care.

An important use of these assertions is in **providing information to the compiler** which it may not be able to infer from the program (either because the information is not present or because the analyzer being used is not precise enough). In particular, providing information on external predicates which may not be accessible at the time of compiling the module can greatly improve the precision of the analyzer. This can be easily done with trust assertion.

### `true/1`

**true assertion** - This assertion is identical syntactically to a `check/1` assertion. However, the properties stated have been proved to hold by the analyzer. Thus, these assertions often represent the **analyzer output**.

### `false/1`

**false assertion** - This assertion is identical syntactically to a `check/1` assertion. However, the properties stated have been proved not to hold by the analyzer. Thus, these assertions often represent the **analyzer output**.
