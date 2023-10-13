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

      case lists:member(From, ListOfUsers) of

        true ->
          {reply, ok, {Channel, ListOfUsers}};

        false ->
          NewListOfUsers = [From | ListOfUsers],
          {reply, ok, {Channel, NewListOfUsers}}

      end;

    {From, leave, Channel} ->

      {_, ListOfUsers} = State,

      case lists:member(From, ListOfUsers) of

        true ->
          NewListOfUsers = lists:delete(From, ListOfUsers),
          {reply, ok, {Channel, NewListOfUsers}};

        false ->
          {reply, ok, State}

      end;


    {From, Nick, message_send, Channel, Msg} ->
      {_, UsersInChannel} = State,
      case lists:member(From, UsersInChannel) of
        true ->
          AllUsersWithoutFrom = lists:delete(From, UsersInChannel),
          lists:foreach(fun(Pid) ->
            Pid ! {request, self(), 0, {message_receive, Channel, Nick, Msg}}
                        end, AllUsersWithoutFrom),

          {reply, ok, State};
        false ->
          {exit, user_not_joined, "Reason"}
      end
  end.