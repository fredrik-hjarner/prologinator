:- module(set_pp_flag_verified, [set_pp_flag_verified/2], []).

:- use_module(library(format), [format/2, format/3]).
:- use_module(ciaopp(ciaopp), [set_pp_flag/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2, valid_flag_value/2]).
:- use_module(library(aggregates), [findall/3]).

% Helper predicate to set a flag and verify it was set correctly
set_pp_flag_verified(Flag, Value) :-
    ( valid_flag_value(Flag, Value) ->
        set_pp_flag(Flag, Value),
        ( current_pp_flag(Flag, Value) ->
            true  % Success - flag was set correctly
        ; format(user_error, "ERROR: Failed to set flag ~w to ~w (verification failed)~n", [Flag, Value]),
          halt(1)
        )
    ; findall(ValidValue, valid_flag_value(Flag, ValidValue), ValidValues),
      format(user_error, "ERROR: Invalid value ~w for flag ~w~n", [Value, Flag]),
      format(user_error, "       Valid values are: ~w~n", [ValidValues]),
      halt(1)
    ).

