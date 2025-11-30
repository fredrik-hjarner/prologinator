% Test file for integer division formula using CLPFD constraints
% Tests: DX = (TargetX - CurrentX) // Frames

:- module(test, [test_move_step/4, test_move_to/7, show_expected/4, debug_constraints/4], [clpfd]).

% Test predicate: computes DX using the bounds formula
% DX = (TargetX - CurrentX) // Frames
% test_move_step(TargetX, CurrentX, Frames, DX) :-
%     DiffX #= TargetX - CurrentX,
%     % Set reasonable domain for DX (non-negative only)
%     DX in 0..1000,
%     % DX * Frames <= DiffX (DX is not too big)
%     DX * Frames #=< DiffX,
%     % (DX + 1) * Frames > DiffX (DX is the largest possible)
%     (DX + 1) * Frames #> DiffX,
%     labeling([], [DX]).

% Test predicate: computes both DX and DY
:- use_module(library(clpfd)).

test_move_to(TargetX, TargetY, CurrentX, CurrentY, Frames, DX, DY) :-
    % 1. Constrain variables using domain/3
    % We constrain Frames to be strictly positive so inequalities don't flip
    domain([Frames], 1, 100),
    
    % Constrain the output/input variables to a reasonable grid size
    domain([TargetX, TargetY, CurrentX, CurrentY, DX, DY], -100, 100),

    DiffX #= TargetX - CurrentX,
    DiffY #= TargetY - CurrentY,

    % 2. Use the adapter to solve for X and Y
    solve_shifted(DiffX, Frames, DX),
    solve_shifted(DiffY, Frames, DY),

    labeling([], [DX, DY, Frames]).

% The Adapter: Shifts negative numbers to positive, solves, then shifts back
solve_shifted(Diff, Frames, Step) :-
    Offset = 200, % A constant large enough to make the math positive
    
    % PosStep is the calculation happening in "positive space"
    domain([PosStep], 0, 1000),

    % Shifted Logic:
    % We want: Step = Diff / Frames
    % We do:   (Step + Offset) = (Diff + Offset*Frames) / Frames
    
    OffsetTerm #= Offset * Frames,
    ShiftedDiff #= Diff + OffsetTerm,

    % Unpack (PosStep + 1) to avoid Ciao syntax errors in constraints
    PosStepNext #= PosStep + 1,

    % Apply your original formula on the POSITIVE variables
    PosStep * Frames      #=< ShiftedDiff,
    PosStepNext * Frames  #>  ShiftedDiff,

    % Convert back to the signed Step
    Step #= PosStep - Offset.

% Helper: show what the actual integer division would be
% show_expected(TargetX, CurrentX, Frames, Expected) :-
%     DiffX is TargetX - CurrentX,
%     Expected is DiffX // Frames.

% Debug version: test constraints without labeling
% debug_constraints(TargetX, CurrentX, Frames, DX) :-
%     DiffX #= TargetX - CurrentX,
%     DX in -1000..1000,
%     DX * Frames #=< DiffX,
%     (DX + 1) * Frames #> DiffX,
%     % Just check if constraints are satisfiable, don't label
%     fd_domain(DX, Min, Max),
%     write('DX domain: '), write(Min), write('..'), write(Max), nl,
%     write('DiffX = '), write(DiffX), nl,
%     write('Frames = '), write(Frames), nl.

% Example queries to try:
% ?- test_move_step(10, 0, 3, DX).          % Should give DX = 3
% ?- test_move_step(7, 0, 3, DX).            % Should give DX = 2
% ?- test_move_step(10, 0, 3, DX), show_expected(10, 0, 3, E).  % Compare
% ?- test_move_to(10, 20, 0, 0, 3, DX, DY).  % Should give DX=3, DY=6

