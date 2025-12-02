:- module(check_assertions, [check_assertions_and_exit/1], []).

:- use_module(library(lists), [member/2]).

% Check assertion counts and exit with appropriate code
% Exit with code 1 if any false or unverified assertions found, 0 otherwise
check_assertions_and_exit(ACheckInfo) :-
    % Extract assertion checking counts
    member(assert_count(_Mod, CTInfo), ACheckInfo),
    member((pp_false_c, [[CallSiteFalseCalls|_]]), CTInfo),
    member((pp_false_s, [[CallSiteFalseSuccess|_]]), CTInfo),
    member((simp_false_c, [[PredicateFalseCalls|_]]), CTInfo),
    member((simp_false_s, [[PredicateFalseSuccess|_]]), CTInfo),
    member((pp_check_c, [[CallSiteUnverifiedCalls|_]]), CTInfo),
    member((pp_check_s, [[CallSiteUnverifiedSuccess|_]]), CTInfo),
    member((simp_check_c, [[PredicateUnverifiedCalls|_]]), CTInfo),
    member((simp_check_s, [[PredicateUnverifiedSuccess|_]]), CTInfo),
    ( (CallSiteFalseCalls > 0
      ; CallSiteFalseSuccess > 0
      ; PredicateFalseCalls > 0
      ; PredicateFalseSuccess > 0
      ; CallSiteUnverifiedCalls > 0
      ; CallSiteUnverifiedSuccess > 0
      ; PredicateUnverifiedCalls > 0
      ; PredicateUnverifiedSuccess > 0) ->
        halt(1)
    ; halt(0)
    ).

