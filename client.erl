-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
  gui, % atom of the GUI process
  nick, % nick/username of the client
  server, % atom of the chat server
  channel
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
  #client_st{
    gui = GUIAtom,
    nick = Nick,
    server = ServerAtom,
    channel = []
  }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Function to handle various operations: Joining a channel, Leaving a channel, and Sending a message.

% Handle request to join a channel.
handle(St, {join, Channel}) ->

  % Check if the server process is running using the client state.
  case whereis(St#client_st.server) of

    % No server process is currently active.
    undefined ->
      {reply, {error, server_not_reached, "no server active to join"}, St};

    % Server process is active and identified by its Pid.
    Pid ->

      % Check if the client has already joined the channel.
      case lists:member(Channel, St#client_st.channel) of

        % Client is already in the channel.
        true ->
          {reply, {error, user_already_joined, "user already joined"}, St};

        % Client has not yet joined the channel.
        false ->
          % Try to join the channel via the server.
          try
            % Request the server to join the channel.
            case genserver:request(Pid, {self(), join, Channel}) of
              ok ->
                % Successfully joined. Update the client's list of channels.
                New_Channels = [Channel | St#client_st.channel],
                New_state = St#client_st{channel = New_Channels},
                {reply, ok, New_state};

              % Handling server exit.
              {'EXIT', _} ->
                {reply, {error, server_not_reached, "server timeout"}, St}
            end

            % Handling timeout error.
          catch
            timeout_error ->
              {reply, {error, server_not_reached, "server timeout"}, St}
          end
      end
  end;

% Handle request to leave a channel.
handle(St, {leave, Channel}) ->

  % Check if the client is a member of the channel.
  case lists:member(Channel, St#client_st.channel) of

    % Client is in the channel.
    true ->

      % Try to leave the channel via the server.
      try
        % Request the server to leave the channel.
        case genserver:request(list_to_atom(Channel), {self(), leave, Channel}) of
          ok ->
            % Successfully left. Update the client's list of channels.
            NewChannels = lists:delete(Channel, St#client_st.channel),
            NewState = St#client_st{channel = NewChannels},
            {reply, ok, NewState};

          % Handling server exit.
          {'EXIT', _} ->
            {reply, {error, server_not_reached, "server timeout"}, St}
        end

        % Handling timeout error.
      catch
        timeout_error ->
          {reply, {error, server_not_reached, "server timeout"}, St}
      end;

    % Client is not in the channel.
    false ->
      {reply, {error, user_not_joined, "user not in channel"}, St}
  end;

% Handle sending a message to a channel.
handle(St, {message_send, Channel, Msg}) ->

  % Check if the server for the specific channel is running.
  case whereis(list_to_atom(Channel)) of

    % The server for the channel is not active.
    undefined ->
      {reply, {error, server_not_reached, "server not up"}, St};

    % The server for the channel is active.
    _ ->

      % Check if the client is a member of the channel.
      case lists:member(Channel, St#client_st.channel) of

        % Client is in the channel.
        true ->

          % Try to send a message to the channel via the server.
          case genserver:request(list_to_atom(Channel), {self(), St#client_st.nick, message_send, Channel, Msg}) of
            ok ->
              {reply, ok, St};

            % Handling server exit.
            {'EXIT', _} ->
              {reply, {error, server_not_reached, "Fel i servern"}, St}
          end;

        % Client is not in the channel.
        false ->
          {reply, {error, user_not_joined, "user not in channel"}, St}
      end
  end;



% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
  {reply, ok, St#client_st{nick = NewNick}};

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St};

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
%%  io:format("~nIn the client for PID: ~p~nMessage recived from: ~p~nMessage: ~p ",[self(), Nick, Msg]),
  gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
  {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
  % Any cleanup should happen here, but this is optional
  {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, Data) ->
  {reply, {error, not_implemented, "Client does not handle this command"}, St}.
