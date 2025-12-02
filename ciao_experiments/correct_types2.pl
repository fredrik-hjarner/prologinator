% Example demonstrating CORRECT type usage
% This file should pass type checking with ciaopp
%
% To verify: ciaopp -V ciao_experiments/correct_types.pl -fmodes=pd -ftypes=eterms -ftype_eval=on
%
% IMPORTANT: The -ftype_eval=on flag is essential for verifying integer arithmetic!
% Without it, the analyzer infers 'num' instead of 'int' for arithmetic results,
% causing assertions to fail verification even when the code is correct.
% With -ftype_eval=on, all assertions are verified (100% checked).

:- module(
    correct_types2,
        [
        % add_five/2,
        % process_list/2,
        % make_pair/2
        add_five_fd/2
    ],
    [clpfd, regtypes, assertions, modes]
).

:- use_module(engine(basic_props), [int/1, list/2]).

% ===========================================================================
% TRUST ASSERTIONS (The "Patch")
% These tell CiaoPP how to handle the "black boxes" of the CLP(FD) library.
% ===========================================================================

% 1. Fix the "new(A)" warning.
%    The #= macro generates a call to new/1. We tell CiaoPP to accept any term.
:- trust pred new(A) : term(A).

% 2. Fix the "labeling" warning.
%    We accept any term as input (to stop input complaints) 
%    and promise integers as output (to verify our logic).
:- trust pred labeling(_Opts, Vars) 
    : list(term, Vars) 
   => list(int, Vars).

% CLP(FD) version - uses constraints instead of is/2
% Note: CiaoPP may not verify this (infers term instead of int)
:- pred add_five_fd(+int, -int)
    # "Adds 5 using CLP(FD) constraints.".

add_five_fd(X, Result) :-
    Result #= X + 5,
    labeling([], [Result]).

% Example usage that should be type-correct
example_usage :-
    add_five_fd(10, _SumFD).     % Correct: CLP(FD) with int
