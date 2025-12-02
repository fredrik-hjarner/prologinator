% CiaoPP Configuration Script
% This script sets up strict analysis flags and runs analysis
% 
% Usage (Recommended - Compile to executable):
%   ciaoc -x check check.pl
%   ./check ciao_experiments/correct_types2.pl
%
% Note: The "WARNING: Source used as module without module declaration" 
% message during compilation is from a dependency file (ciaopp_options.itf)
% and can be safely ignored. The executable works correctly.

:- module(ciaopp, [main/1], []).

:- use_module(ciaopp(ciaopp)).
:- use_module(ciaopp(ciaopp_options)).
:- use_module(ciaopp(analyze_driver), [acheck_summary/2]).
:- use_module(ciaopp(analysis_stats), [pretty_print_acheck_stats/1]).
:- use_module(set_pp_flag_verified, [set_pp_flag_verified/2]).
:- use_module(check_assertions, [check_assertions_and_exit/1]).

main([FileToAnalyze]) :-
    % ---------------------------------------------------------
    % 1. THE ENGINE (The Brain)
    % ---------------------------------------------------------
    % Use the modern analysis engine (Programming in Logic w/ Abstract Interpretation)
    set_pp_flag_verified(fixpoint, plai),
    % Analyze variable instantiation (Ground/Free)
    set_pp_flag_verified(modes, pd),
    % Analyze types (List, Int, Term)
    set_pp_flag_verified(types, eterms),

    % ---------------------------------------------------------
    % 2. STRICTNESS (The Linter)
    % ---------------------------------------------------------
    % Treat unproven checks as ERRORS (Exit code failure)
    set_pp_flag_verified(asr_not_stat_eval, error),
    % Verify predicate contracts (:- pred)
    set_pp_flag_verified(pred_ctchecks, on),
    % Verify every single line of code inside functions
    set_pp_flag_verified(pp_ctchecks, on),
    % Calculate arithmetic (5+5=10) instead of treating it as symbols
    set_pp_flag_verified(type_eval, on),

    % ---------------------------------------------------------
    % 3. PRECISION (Context Sensitivity)
    % ---------------------------------------------------------
    % If a function is called in 2 different ways, check both separately.
    % (Like checking foo<string> and foo<int> separately)
    set_pp_flag_verified(multivariant_ctchecks, on),
    
    % ---------------------------------------------------------
    % 4. COVERAGE (Private Code)
    % ---------------------------------------------------------
    % Check EVERYTHING. Even private helpers that aren't exported.
    % Forces you to type-check your internal code.
    set_pp_flag_verified(entry_points_auto, all),  % Valid: none, calls, all

    % ---------------------------------------------------------
    % 5. EXECUTION
    % ---------------------------------------------------------
    module(FileToAnalyze),
    
    % [NEW] Analyze Sharing/Freeness (Pointers/Aliasing)
    % This helps 'pd' be more accurate about Groundness.
    % format("Analyzing: Sharing/Freeness (shfr)...~n", []),
    analyze(shfr),

    % format("Analyzing: Modes (pd)...~n", []),
    analyze(pd),

    % format("Analyzing: Types (eterms)...~n", []),
    analyze(eterms),
    
    % Get stats and print summary
    acheck_summary(ACheckInfo, _Summary),
    pretty_print_acheck_stats(ACheckInfo),

    % ---------------------------------------------------------
    % 6. CI/CD EXIT CODE (The "Fail" Logic)
    % ---------------------------------------------------------
    check_assertions_and_exit(ACheckInfo).
