% RNG predicates with cyclic index
% 256-value table from rng_table.pl

% ==============================================
% rng_next(IndexIn, FloatOut, IndexOut)
% ==============================================
% Get next float [0, 1) and advance index
% Wraps at 256

rng_next(I, F, I1) :-
    rng_val(I, F),
    I1 #= (I + 1) mod 256.

% ==============================================
% rng_int_range(Min, Max, IndexIn, 
%               ValueOut, IndexOut)
% ==============================================
% Get random integer in [Min, Max] inclusive
% Respects 60-char line limit

rng_int_range(Min, Max, I, Val, I1) :-
    rng_next(I, F, I1),
    Range is Max - Min + 1,
    Scaled is F * Range,
    FloorScaled is floor(Scaled),
    Val is Min + FloorScaled.
