# Modes Implementation

NOTE: Should we also add indeps via ivar/1 (i.e., use ivar in all places where we have var, since that is normally the intended meaning). Check in practice.

## ISO-like modes

Note that, if nonvar in calls is included in the parametric versions —not done since implied by instantiation type— then these modes are equivalent to parametric versions with P = term.

```prolog
:- modedef  +(A) :  nonvar(A).
:- modedef  -(A) => nonvar(A). % This was : var(A). Could also imply steadfast.
:- modedef --(A) :  var(A).
:- modedef  ?(_).
:- modedef  @(A) +  not_further_inst(A).
```

## Useful input-output modes

```prolog
:- modedef in(A)  : ground(A) => ground(A).
:- modedef ++(A)  : ground(A) => ground(A).  % Optional alias...
:- modedef out(A) : var(A)    => ground(A).
:- modedef go(A)              => ground(A).  % Note: used in foreign interface
```

TODO: Other possibilities:
- `: mode` for meta (also implies nonvar)
- `! mode` for mutables (cf. fsyntaxplus)

## Parametric versions

TODO: hiord order? (Need to change in assrt)

```prolog
:- modedef  +(A,P) : (nonvar(A),P(A)).           % Obviously implies nonvar(A)
:- modedef  -(A,P)          => (nonvar(A),P(A)).
:- modedef --(A,P) : var(A) => P(A).
:- modedef  ?(A,P) :: P(A).
:- modedef  @(A,P) :: P(A) + not_further_inst(A).

:- modedef in(A,P)  : (ground(A),P(A)) => ground(A).
:- modedef ++(A,P)  : (ground(A),P(A)) => ground(A). % Optional alias...
:- modedef out(A,P) : var(A) => (ground(A),P(A)).
:- modedef go(A,P)           => (ground(A),P(A)).
```

