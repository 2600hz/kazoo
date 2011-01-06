% Copyright 2010, Scatterbit, Inc. & Jayson Vantuyl <jvantuyl@scatterbit.com>
%
% This file is part of erlctl.
%
% Erlctl is open source.  See LICENSE.txt for details.
-module(ctl_ex).
-behavior(gen_server).
-export([start_link/0,add_user/1,del_user/1,list_users/0]).
-export([init/1,terminate/2,code_change/3]).
-export([handle_call/3,handle_cast/2,handle_info/2]).

-record(ce_state,{users}).

%% API
start_link() ->   gen_server:start_link({local,?MODULE},?MODULE,[],[]).
add_user(User) -> gen_server:call(?MODULE,{add_user,User}).
del_user(User) -> gen_server:call(?MODULE,{del_user,User}).
list_users()   -> gen_server:call(?MODULE,list_users).

%% Server Implementation
init(_) -> {ok,#ce_state{users = sets:new()}}.

terminate(_Reason,_State) -> stopped.

code_change(_OldVsn,OldState,_Extra) ->
  NewState = OldState,
  {ok,NewState}.

handle_call({add_user,User},_From,State) ->
  Users = State#ce_state.users,
  case sets:is_element(User,Users) of
    true ->
      {reply,user_exists,State};
    false ->
      NewUsers = sets:add_element(User,Users),
      NewState = State#ce_state{users=NewUsers},
      {reply,ok,NewState}
  end;
handle_call({del_user,User},_From,State) ->
  Users = State#ce_state.users,
  case sets:is_element(User,Users) of
    true ->
      NewUsers = sets:del_element(User,Users),
      NewState = State#ce_state{users=NewUsers},
      {reply,ok,NewState};
    false ->
      {reply,no_such_user,State}
  end;
handle_call(list_users,_From,State) ->
  Users = State#ce_state.users,
  UserList = lists:sort(sets:to_list(Users)),
  {reply,{ok,UserList},State}.

handle_cast(notimplemented,State) -> {noreply,State}.

handle_info(notimplemented,State) -> {noreply,State}.
