:- module(run_tests, [extract_module_name/2], []).

:- use_module(library(unittest), [run_tests_in_module/1]).
:- use_module(library(system), [getenvstr/2]).

% Extract module name from path (e.g., "prolog/engine" -> "engine")
extract_module_name(Path, ModuleName) :-
    (atom_concat(_, '/', Path) ->
        % Has a '/', recursively extract from the part after '/'
        atom_concat(_, '/', Path),
        atom_concat(_, Rest, Path),
        extract_module_name(Rest, ModuleName)
    ;
        % No '/', this is the module name
        ModuleName = Path
    ).

:- initialization((
    getenvstr('TEST_MODULE', ModuleStr),
    % Convert string to atom
    atom_codes(ModuleAtom, ModuleStr),
    % Remove .pl extension if present
    (atom_concat(ModuleName, '.pl', ModuleAtom) ->
        ModuleNameNoExt = ModuleName
    ;
        ModuleNameNoExt = ModuleAtom
    ),
    % Extract module name from path (handles both "engine" and "prolog/engine")
    extract_module_name(ModuleNameNoExt, FinalModuleName),
    run_tests_in_module(FinalModuleName)
)).

