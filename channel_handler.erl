%%%-------------------------------------------------------------------
%%% @author attil
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2023 14:54
%%%-------------------------------------------------------------------
-module(channel_handler).
-author("attil").

%% API
-export([channel_handler/2]).


% Main function to handle different channel-related actions.
channel_handler(State, Data) ->
  case Data of
    % Handling a request for a user to join a channel.
    {From, join, Channel} ->
      % Extracting the list of users from the State.
      {_, ListOfUsers} = State,

      % Check if the user is already a member of the channel.
      case lists:member(From, ListOfUsers) of
        true ->
          % User is already in the channel, return the current state.
          {reply, ok, {Channel, ListOfUsers}};
        false ->
          % Add the user to the list and return the updated state.
          NewListOfUsers = [From | ListOfUsers],
          {reply, ok, {Channel, NewListOfUsers}}
      end;

    % Handling a request for a user to leave a channel.
    {From, leave, Channel} ->
      {_, ListOfUsers} = State,

      % Check if the user is a member of the channel.
      case lists:member(From, ListOfUsers) of
        true ->
          % Remove the user from the list and return the updated state.
          NewListOfUsers = lists:delete(From, ListOfUsers),
          {reply, ok, {Channel, NewListOfUsers}};
        false ->
          % User is not in the channel, return the current state.
          {reply, ok, State}
      end;

    % Handling a user sending a message in a channel.
    {From, Nick, message_send, Channel, Msg} ->
      {_, UsersInChannel} = State,

      % Check if the message sender is a member of the channel.
      case lists:member(From, UsersInChannel) of
        true ->
          % Send the message to all users in the channel except the sender.
          AllUsersWithoutFrom = lists:delete(From, UsersInChannel),
          lists:foreach(fun(Pid) ->
            Pid ! {request, self(), 0, {message_receive, Channel, Nick, Msg}}
                        end, AllUsersWithoutFrom),

          % Return the current state.
          {reply, ok, State};
        false ->
          % Exit if the user is not a member of the channel.
          {exit, user_not_joined, "user is not a member of the channel"}
      end
  end.