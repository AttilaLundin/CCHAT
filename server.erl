-module(server).
-import(channel_handler, [channel_handler/2]).
-export([start/1,stop/1, handler/2]).


% Starts a new server process with the provided name (ServerAtom).
% If a process with that name already exists, it's unregistered first.
% Do not change the signature of this function.
start(ServerAtom) ->

    % Unregister the process if one exists with the given name.
    % The catch is used to suppress any error that might occur if the server is not registered.
    catch(unregister(ServerAtom)),

    % Start a new generic server with the specified name, initial state, and a handler function.
    Pid = genserver:start(ServerAtom, [], fun handler/2),

    % Return the Pid of the newly started server.
    Pid.

% Handler function to manage the state and data of the server.
handler(State, Data) ->
    case Data of
        % Handle a request to join a channel.
        {From, join, Channel} ->
            % Check if the channel is already in the state.
            case lists:member(Channel, State) of
                % If the channel is in the state, send a join request to the channel.
                true ->
                    genserver:request(list_to_atom(Channel), {From, join, Channel}),
                    {reply, ok, State};

                % If the channel is not in the state, start a new generic server for the channel.
                % Then, add the channel to the server's state.
                false ->
                    genserver:start(list_to_atom(Channel), {Channel, [From]}, fun channel_handler:channel_handler/2),
                    NewState = [Channel | State],
                    {reply, ok, NewState}
            end;

        % Handle a request to stop the server.
        {stop} ->

            % For each channel in the server's state, stop the associated generic server.
            lists:foreach(fun(Channel) ->
                genserver:stop(list_to_atom(Channel))
                          end, State),
            % Reply with 'ok' and an empty list, indicating no channels are in the state anymore.
            {reply, ok, []}
    end.

% Stop the server process associated with the given name (ServerAtom).
% This also stops any other processes associated with this server.
stop(ServerAtom) ->
    % Send a stop request to the server to initiate its shutdown sequence.
    genserver:request(ServerAtom, {stop}),

    % Stop the generic server associated with the given name.
    genserver:stop(ServerAtom).