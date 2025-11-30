:- module(run_tests, [], []).

:- use_module(library(unittest), [run_tests_in_module/1]).
:- use_module(library(system), [getenvstr/2]).
:- use_module(engine(atomic_basic), [atom_number/2]).

:- initialization((
    (getenvstr('TEST_MODULE', ModuleStr) ->
        % Convert string to atom
        atom_codes(ModuleAtom, ModuleStr),
        % Remove .pl extension if present, run_tests_in_module expects module name
        (atom_concat(ModuleName, '.pl', ModuleAtom) ->
            run_tests_in_module(ModuleName)
        ;
            run_tests_in_module(ModuleAtom)
        )
    ;
        % Default: run game tests if no module specified
        run_tests_in_module(game)
    )
)).

