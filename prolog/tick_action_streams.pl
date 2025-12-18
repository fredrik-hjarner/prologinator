% ==========================================================
% Execution Model: tick_action_streams
% ==========================================================

% tick_action_streams threads the context via DCG.
% Takes an object ID and processes all action streams for
% that object.
% tick_action_streams(+ObjectID)
tick_action_streams(ObjectID) -->
    ctx_actionstore(ActionStoreIn),
    ( {gen_assoc(ObjectID, ActionStoreIn, ActionStreams)} ->
        % Process streams and get updated list
        tick_action_streams_loop(
            ObjectID, ActionStreams, ActionStreamsOut
        ),
        % Remove empty/completed streams
        {filter_empty_streams(ActionStreamsOut,
                               FilteredStreams)},
        % Update actionstore with filtered streams
        {put_assoc(ObjectID, ActionStoreIn, FilteredStreams,
                   ActionStoreOut)},
        ctx_set_actionstore(ActionStoreOut)
    ;
        % Object not in actionstore - nothing to do
        []
    ).

% tick_action_streams_loop(+ObjectID, +StreamsIn,
%   -StreamsOut)
% Processes streams in order until one despawns or all
%   complete
tick_action_streams_loop(_ObjectID, [], []).
tick_action_streams_loop(ObjectID, [Stream|Rest],
                          StreamsOut) -->
    tick_action_stream(ObjectID, Stream, Result),
    tick_action_streams_handle_result(
        Result, ObjectID, Rest, StreamsOut
    ).

% tick_action_streams_handle_result(+Result, +ObjectID,
%   +Rest, -StreamsOut)
% Handles the result from tick_action_stream
tick_action_streams_handle_result(
    result(despawned, _), _ObjectID, Rest, StreamsOut
) -->
    {StreamsOut = Rest}.
tick_action_streams_handle_result(
    result(completed, NewStream), ObjectID, Rest, StreamsOut
) -->
    tick_action_streams_loop(ObjectID, Rest, RestOut),
    {StreamsOut = [NewStream|RestOut]}.
tick_action_streams_handle_result(
    result(yielded, NewStream), ObjectID, Rest, StreamsOut
) -->
    tick_action_streams_loop(ObjectID, Rest, RestOut),
    {StreamsOut = [NewStream|RestOut]}.

% filter_empty_streams(+StreamsIn, -StreamsOut)
% Removes empty/completed streams (lists that are empty)
filter_empty_streams([], []).
filter_empty_streams([[]|Rest], Filtered) :-
    filter_empty_streams(Rest, Filtered).
filter_empty_streams([Stream|Rest], [Stream|Filtered]) :-
    Stream \= [],
    filter_empty_streams(Rest, Filtered).

