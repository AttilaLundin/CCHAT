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

channel_handler(State, Data) ->
  case Data of
    {From, join, Channel} ->
      {_, ListOfUsers} = State,
      NewListOfUsers = [From | ListOfUsers],
      {reply, ok, {Channel, NewListOfUsers}};
    {From, leave, Channel} ->
      {_, ListOfUsers} = State,
      case lists:member(From, ListOfUsers) of
        false -> {reply, ok, {Channel, ListOfUsers}};
        true ->
          NewListOfUsers = lists:delete(From, ListOfUsers),
          {reply, ok, {Channel, NewListOfUsers}}
      end;
    {From, message_send, Channel, Msg} ->
      {Channel, ListOfUsers} = State,
      case lists:member(From, ListOfUsers) of
        true -> spawn(fun() -> send_message(From, Channel, ListOfUsers, Msg) end),
          {reply, ok, {Channel, ListOfUsers}};
        false ->
          {exit, user_not_joined, "Reason"}
      end

  end.

send_message(From, Channel, ListOfUsers, Msg) ->
  ListOfUsersWithoutSelf = lists:delete(From, ListOfUsers),
  %send the message to every other client in the channel
  [User ! {request, self(), 0, {message_receive, Channel, User, Msg}} || User <- ListOfUsersWithoutSelf].
