-module(server).
-import(channel_handler, [channel_handler/2]).
-export([start/1,stop/1, handler/2]).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    catch(unregister(ServerAtom)),
    Pid = genserver:start(ServerAtom, [], fun handler/2),
    Pid.


handler(State, {From, join, Channel}) ->
%%    io:format("~n~n~n IN THE HANDLER!!!!!!!!~n~n"),
    case lists:member(Channel, State) of
        true -> genserver:request(list_to_atom(Channel), {From, join, Channel}),
            {reply, ok, State};
        false -> genserver:start(list_to_atom(Channel), {Channel, [From]}, fun channel_handler:channel_handler/2),
            NewState = [Channel | State],
            {reply, ok, NewState}
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
    % TODO Implement function
    % Return ok
%%    not_implemented.
