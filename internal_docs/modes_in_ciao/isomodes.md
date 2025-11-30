# ISO Modes Implementation

The ISO standard is unfortunately not very clear/formal in the description of modes, but these interpretations seem the most sensible.

## Basic ISO-modes

```prolog
:- modedef '+'(A) : nonvar(A).
:- modedef '-'(A) : var(A).
```

The standard says that this should be:
```prolog
% :- modedef '-'(A) : var(A) => nonvar(A).
```
but then it says that the only error possible is for not meeting the `: var...` what to do?

```prolog
:- modedef '?'(_).
:- modedef '@'(A) : nonvar(A) + not_further_inst(A).
```

Only in older versions of standard? It is obsolete now.
```prolog
% :- modedef '*'(_).
```

## Parametric versions

```prolog
:- modedef +(A,X) :  X(A).
:- modedef -(A,X) :  var(A) => X(A).
```

Version in standard supports this simple interpretation:
```prolog
% :- modedef ?(A,X) :: X(A).
```
but all builtins conform to:
```prolog
:- modedef ?(A,X) :: X(A) => X(A).
```
..what to do??

```prolog
:- modedef @(A,X) :  X(A) => X(A) + not_further_inst(A).
```

Only in older versions of standard? It is obsolete now.
```prolog
% :- modedef *(A,X) :: X(A).
```

