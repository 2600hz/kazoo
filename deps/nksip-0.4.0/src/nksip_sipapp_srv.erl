
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @private SipApp core process
%%
%% This module contains the actual implementation of the SipApp's core process

-module(nksip_sipapp_srv).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([get/2, put/3, put_new/3, del/2]).
-export([get_appid/1, get_name/1, config/1, config/2, config/3]).
-export([pending_msgs/0, start_plugins/2, stop_plugins/2]).
-export([get_meta/2, set_meta/3, update_uuid/3]).
-export([start_link/2, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export_type([state/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================


-type state() :: #sipapp_srv{}.


%% ===================================================================
%% Private
%% ===================================================================


%% @doc Gets a value from SipApp's store
-spec get(nksip:app_id(), term()) ->
    {ok, term()} | undefined | {error, term()}.

get(AppId, Key) ->
    case catch ets:lookup(AppId, Key) of
        [{_, Value}] -> {ok, Value};
        [] -> undefined;
        _ -> {error, ets_error}
    end.


%% @doc Inserts a value in SipApp's store
-spec put(nksip:app_id(), term(), term()) ->
    ok | {error, term()}.

put(AppId, Key, Value) ->
    case catch ets:insert(AppId, {Key, Value}) of
        true -> ok;
        _ -> {error, ets_error}
    end.


%% @doc Deletes a value from SipApp's store
-spec del(nksip:app_id(), term()) ->
    ok | {error, term()}.

del(AppId, Key) ->
    case catch ets:delete(AppId, Key) of
        true -> ok;
        _ -> {error, ets_error}
    end.


%% @doc Inserts a value in SipApp's store
-spec put_new(nksip:app_id(), term(), term()) ->
    true | false | {error, term()}.

put_new(AppId, Key, Value) ->
    case catch ets:insert_new(AppId, {Key, Value}) of
        true -> true;
        false -> false;
        _ -> {error, ets_error}
    end.


%% @doc Generates a internal name (an atom()) for any term
-spec get_appid(nksip:app_name()) ->
    nksip:app_id().

get_appid(AppName) ->
    list_to_atom(
        string:to_lower(
            case binary_to_list(nksip_lib:hash36(AppName)) of
                [F|Rest] when F>=$0, F=<$9 -> [$A+F-$0|Rest];
                Other -> Other
            end)).


%% @doc Finds the internal name corresponding to any user name
-spec get_name(nksip:app_id()) ->
    nksip:app_name().

get_name(AppId) ->
    AppId:name().


%% @doc Gets the sipapp's configuration
-spec config(nksip:app_id()) ->
    nksip:optslist().

config(AppId) ->
    AppId:config().


%% @doc Gets a value from the sipapp's configuration
-spec config(nksip:app_id(), term()) ->
    term() | undefined.

config(AppId, Key) ->
    nksip_lib:get_value(Key, AppId:config()).


%% @doc Gets a value from the sipapp's configuration with a default
-spec config(nksip:app_id(), term(), term()) ->
    term().

config(AppId, Key, Default) ->
    case config(AppId, Key) of
        undefined -> Default;
        Value -> Value
    end.


%% @private
pending_msgs() ->
    lists:map(
        fun({Name, Pid}) ->
            {_, Len} = erlang:process_info(Pid, message_queue_len),
            {Name, Len}
        end,
        nksip_proc:values(nksip_sipapps)).


%% @private
get_meta(Key, #sipapp_srv{meta=Meta}) -> 
    case lists:keyfind(Key, 1, Meta) of
        false -> undefined;
        {_, Value} -> Value
    end.


%% @private
set_meta(Key, undefined, #sipapp_srv{meta=Meta}=State) ->
    Meta1 = lists:keydelete(Key, 1, Meta),
    State#sipapp_srv{meta=Meta1};

set_meta(Key, Value, #sipapp_srv{meta=Meta}=State) ->
    Meta1 = lists:keystore(Key, 1, Meta, {Key, Value}),
    State#sipapp_srv{meta=Meta1}.


%% @private
start_plugins(AppId, Plugins) ->
    Msg = {'$nksip_sipapp_srv_start_plugins', Plugins},
    gen_server:call(AppId, Msg, 30000).


%% @private
stop_plugins(AppId, Plugins) ->
    Msg = {'$nksip_sipapp_srv_stop_plugins', Plugins},
    gen_server:call(AppId, Msg, 30000).


%% ===================================================================
%% gen_server
%% ===================================================================


%% @private
start_link(AppId, Args) -> 
    gen_server:start_link({local, AppId}, ?MODULE, [AppId, Args], []).


%% @private
init([AppId, Args]) ->
    process_flag(trap_exit, true),          % Allow receiving terminate/2
    nksip_proc:put(nksip_sipapps, AppId),   
    Config = AppId:config(),
    AppName = nksip_lib:get_value(name, Config),
    true = nksip_proc:reg({nksip_sipapp_name, AppName}, AppId), 
    Plugins = AppId:config_plugins(),
    State = #sipapp_srv{
        app_id = AppId, 
        args = Args, 
        meta = []
    },
    #sipapp_srv{args=Args1} = State1 = do_start_plugins(Plugins, State),
    case erlang:function_exported(AppId, init, 1) andalso AppId:init(Args1) of
        {ok, ModState} -> 
            {ok, State1#sipapp_srv{sipapp_state=ModState}};
        {ok, ModState, Timeout} -> 
            {ok, State1#sipapp_srv{sipapp_state=ModState}, Timeout};
        {stop, Reason} -> 
            {stop, Reason};
        false ->
            {ok, State1#sipapp_srv{sipapp_state=undefined}}
    end.



%% @private
-spec handle_call(term(), from(), #sipapp_srv{}) ->
    gen_server_call(#sipapp_srv{}).

handle_call({'$nksip_sipapp_srv_start_plugins', Plugins}, _From, State) ->
    State1 = do_start_plugins(Plugins, State),
    {reply, ok, State1};

handle_call({'$nksip_sipapp_srv_stop_plugins', Plugins}, _From, State) ->
    State1 = do_stop_plugins(Plugins, State),
    {reply, ok, State1};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(Msg, From, #sipapp_srv{app_id=AppId}=State) ->
    case AppId:nkcb_handle_call(Msg, From, State) of
        {continue, [Msg1, From1, State1]} ->
            mod_handle_call(Msg1, From1, State1);
        {ok, State1} ->
            {noreply, State1}
    end.


%% @private
-spec handle_cast(term(), #sipapp_srv{}) ->
    gen_server_cast(#sipapp_srv{}).

handle_cast(Msg, #sipapp_srv{app_id=AppId}=State) -> 
    case AppId:nkcb_handle_cast(Msg, State) of
        {continue, [Msg1, State1]} -> 
            mod_handle_cast(Msg1, State1);
        {ok, State1} -> 
            {noreply, State1}
    end.


%% @private
-spec handle_info(term(), #sipapp_srv{}) ->
    gen_server_info(#sipapp_srv{}).

handle_info(Msg, #sipapp_srv{app_id=AppId}=State) -> 
    case AppId:nkcb_handle_info(Msg, State) of
        {continue, [Msg1, State1]} -> 
            mod_handle_info(Msg1, State1);
        {ok, State1} -> 
            {noreply, State1}
    end.


%% @private
-spec code_change(term(), #sipapp_srv{}, term()) ->
    gen_server_code_change(#sipapp_srv{}).

code_change(OldVsn, #sipapp_srv{app_id=AppId, sipapp_state=ModState}=State, Extra) ->
    case erlang:function_exported(AppId, code_change, 3) of
        true ->
            {ok, ModState1} = AppId:code_change(OldVsn, ModState, Extra),
            {ok, State#sipapp_srv{sipapp_state=ModState1}};
        false -> 
            {ok, State}
    end.


%% @private
-spec terminate(term(), #sipapp_srv{}) ->
    gen_server_terminate().

terminate(Reason, State) ->  
    #sipapp_srv{app_id=AppId, sipapp_state=ModState} = State,
    case erlang:function_exported(AppId, terminate, 2) of
        true -> AppId:terminate(Reason, ModState);
        false -> ok
    end,
    do_stop_plugins(lists:reverse(AppId:config_plugins()), State).
    


%% ===================================================================
%% Internal
%% ===================================================================

      
%% @private
do_start_plugins([Plugin|Rest], #sipapp_srv{app_id=AppId}=State) ->
    ?debug(AppId, <<>>, "Starting plugin ~p", [Plugin]),
    case erlang:function_exported(Plugin, init, 2) of
        true ->
            {ok, #sipapp_srv{}=State1} = Plugin:init(AppId, State),
            do_start_plugins(Rest, State1);
        false ->
            do_start_plugins(Rest, State)
    end;

do_start_plugins([], State) ->
    State.


%% @private
do_stop_plugins([Plugin|Rest], #sipapp_srv{app_id=AppId}=State) ->
    ?debug(AppId, <<>>, "Stopping plugin ~p", [Plugin]),
    case erlang:function_exported(Plugin, terminate, 2) of
        true ->
            {ok, #sipapp_srv{}=State1} = Plugin:terminate(AppId, State),
            do_stop_plugins(Rest, State1);
        false ->
            do_stop_plugins(Rest, State)
    end;

do_stop_plugins([], State) ->
    State.


%% @private
-spec mod_handle_call(term(), from(), #sipapp_srv{}) -> 
    {noreply, #sipapp_srv{}, non_neg_integer()} |
    {stop, term(), #sipapp_srv{}}.
    

mod_handle_call(Msg, From, #sipapp_srv{app_id=AppId, sipapp_state=ModState}=State) ->
    case AppId:handle_call(Msg, From, ModState) of
        {reply, Reply, ModState1} -> 
            gen_server:reply(From, Reply),
            {noreply, State#sipapp_srv{sipapp_state=ModState1}};
        {reply, Reply, ModState1, Timeout} -> 
            gen_server:reply(From, Reply),
            {noreply, State#sipapp_srv{sipapp_state=ModState1}, Timeout};
        {noreply, ModState1} -> 
            {noreply, State#sipapp_srv{sipapp_state=ModState1}};
        {noreply, ModState1, Timeout} -> 
            {noreply, State#sipapp_srv{sipapp_state=ModState1}, Timeout};
        {stop, Reason, ModState1} -> 
            {stop, Reason, State#sipapp_srv{sipapp_state=ModState1}}
    end.


%% @private
-spec mod_handle_cast(term(), #sipapp_srv{}) -> 
    {noreply, #sipapp_srv{}, non_neg_integer()} |
    {stop, term(), #sipapp_srv{}}.

mod_handle_cast(Msg, #sipapp_srv{app_id=AppId, sipapp_state=ModState}=State) ->
    case AppId:handle_cast(Msg, ModState) of
        {noreply, ModState1} -> 
            {noreply, State#sipapp_srv{sipapp_state=ModState1}};
        {noreply, ModState1, Timeout} -> 
            {noreply, State#sipapp_srv{sipapp_state=ModState1}, Timeout};
        {stop, Reason, ModState1} -> 
            {stop, Reason, State#sipapp_srv{sipapp_state=ModState1}}
    end.


%% @private
-spec mod_handle_info(term(), #sipapp_srv{}) ->
    {noreply, #sipapp_srv{}, non_neg_integer()} |
    {error, term(), #sipapp_srv{}}.

mod_handle_info(Info, #sipapp_srv{app_id=AppId, sipapp_state=ModState}=State) ->
    case erlang:function_exported(AppId, handle_info, 2) of
        true ->
            case AppId:handle_info(Info, ModState) of
                {noreply, ModState1} -> 
                    {noreply, State#sipapp_srv{sipapp_state=ModState1}};
                {noreply, ModState1, Timeout} -> 
                    {noreply, State#sipapp_srv{sipapp_state=ModState1}, Timeout};
                {stop, Reason, ModState1} -> 
                    {stop, Reason, State#sipapp_srv{sipapp_state=ModState1}}
            end;
        false ->
            case Info of
                {'EXIT', _, normal} -> ok;
                _ -> ?warning(AppId, <<>>, "received unexpected message ~p", [Info])
            end,
            {noreply, State}
    end.




%% @private
update_uuid(AppId, AppName, BasePath) ->
    case read_uuid(AppId, BasePath) of
        {ok, UUID} ->
            ok;
        {error, Path} ->
            UUID = nksip_lib:uuid_4122(),
            save_uuid(Path, AppName, UUID)
    end,
    {ok, UUID}.


%% @private
read_uuid(AppId, BasePath) ->
    Path = filename:join(BasePath, atom_to_list(AppId)++".uuid"),
    case file:read_file(Path) of
        {ok, Binary} ->
            case binary:split(Binary, <<$,>>) of
                [UUID|_] when byte_size(UUID)==36 -> {ok, UUID};
                _ -> {error, Path}
            end;
        _ -> 
            {error, Path}
    end.


%% @private
save_uuid(Path, AppId, UUID) ->
    Content = [UUID, $,, nksip_lib:to_binary(AppId)],
    case file:write_file(Path, Content) of
        ok ->
            ok;
        Error ->
            lager:warning("Could not write file ~s: ~p", [Path, Error]),
            ok
    end.



