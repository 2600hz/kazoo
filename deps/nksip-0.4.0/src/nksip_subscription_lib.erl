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

%% @private User Subscriptions Library Module.
-module(nksip_subscription_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_handle/1, parse_handle/1, meta/2, metas/2]).
-export([remote_meta/2, remote_metas/2]).
-export([make_id/1, find/2, state/1, remote_id/2]).

-export_type([id/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type id() :: 
    binary().



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Get the subscripion a request, response or id
-spec get_handle(nksip:subscription()|nksip:request()|nksip:response()|nksip:handle()) ->
    nksip:handle().

get_handle({user_subs, #subscription{id=SubsId}, 
               #dialog{app_id=AppId, id=DialogId, call_id=CallId}}) ->
    App = atom_to_binary(AppId, latin1), 
    <<$U, $_, SubsId/binary, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>;
get_handle(#sipmsg{app_id=AppId, dialog_id=DialogId, call_id=CallId}=SipMsg) ->
    SubsId = make_id(SipMsg),
    App = atom_to_binary(AppId, latin1), 
    <<$U, $_, SubsId/binary, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>;
get_handle(<<"U_", _/binary>>=Id) ->
    Id;
get_handle(_) ->
    error(invalid_subscription).


%% @private
-spec parse_handle(nksip:handle()) ->
    {nksip:app_id(), id(), nksip_dialog_lib:id(), nksip:call_id()}.

parse_handle(<<"U_", SubsId:6/binary, $_, DialogId:6/binary, $_, App:7/binary, 
         $_, CallId/binary>>) ->
    AppId = binary_to_existing_atom(App, latin1),
    {AppId, SubsId, DialogId, CallId};
parse_handle(_) ->
    error(invalid_handle).


%% @doc
-spec meta(nksip_subscription:field(), nksip:subscription()) -> 
    term().

meta(Field, {user_subs, U, D}) ->
    case Field of
        id ->
            get_handle({user_subs, U, D});
        internal_id -> 
            U#subscription.id;
        status -> 
            U#subscription.status;
        event -> 
            U#subscription.event;
        raw_event -> 
            nksip_unparse:token(U#subscription.event);
        class -> 
            U#subscription.class;
        answered -> 
            U#subscription.answered;
        expires when is_reference(U#subscription.timer_expire) ->
            round(erlang:read_timer(U#subscription.timer_expire)/1000);
        expires ->
            undefined;
       _ ->
            nksip_dialog:meta(Field, D)
    end.


-spec metas([nksip_subscription:field()], nksip:subscription()) -> 
    [{nksip_subscription:field(), term()}].

metas(Fields, {user_subs, U, D}) when is_list(Fields) ->
    [{Field, meta(Field, {user_subs, U, D})} || Field <- Fields].


%% @doc Extracts remote meta
-spec remote_meta(nksip_subscription:field(), nksip:handle()) ->
    {ok, term()} | {error, term()}.

remote_meta(Field, Handle) ->
    case remote_metas([Field], Handle) of
        {ok, [{_, Value}]} -> {ok, Value};
        {error, Error} -> {error, Error}
    end.


%% @doc Extracts remote metas
-spec remote_metas([nksip_subscription:field()], nksip:handle()) ->
    {ok, [{nksip_dialog:field(), term()}]} | {error, term()}.

remote_metas(Fields, Handle) when is_list(Fields) ->
    {AppId, SubsId, DialogId, CallId} = parse_handle(Handle),
    Fun = fun(Dialog) ->
        case find(SubsId, Dialog) of
            #subscription{} = U -> 
                case catch metas(Fields, {user_subs, U, Dialog}) of
                    {'EXIT', {{invalid_field, Field}, _}} -> 
                        {error, {invalid_field, Field}};
                    Values -> 
                        {ok, Values}
                end;
            not_found -> 
                {error, invalid_subscription}
        end
    end,
    case nksip_call:apply_dialog(AppId, CallId, DialogId, Fun) of
        {apply, {ok, Values}} -> 
            {ok, Values};
        {apply, {error, {invalid_field, Field}}} -> 
            error({invalid_field, Field});
        {error, Error} -> 
            {error, Error}
    end.



% %% @doc Gets the subscription object corresponding to a request or subscription and a call
% -spec get_subscription(nksip:request()|nksip:response()|nksip:subscription(), nksip:call()) ->
%     {ok, nksip:subscription()} | {error, term()}.

% get_subscription(#sipmsg{}=SipMsg, #call{}=Call) ->
%     case nksip_dialog:get_dialog(SipMsg, Call) of
%         {ok, Dialog} ->
%             SubsId = make_id(SipMsg),
%             case find(SubsId, Dialog) of
%                 #subscription{}=Subs ->
%                     {ok, {user_subs, Subs, Dialog}};
%                 not_found ->
%                     {error, invalid_subscription}
%             end;
%         {error, _} ->
%             {error, invalid_subscription}
%     end.



%% @private
-spec make_id(nksip:request()) ->
    id().

make_id(#sipmsg{class={req, 'REFER'}, cseq={CSeqNum, 'REFER'}}) ->
    nksip_lib:hash({<<"refer">>, nksip_lib:to_binary(CSeqNum)});

make_id(#sipmsg{class={resp, _, _}, cseq={CSeqNum, 'REFER'}}) ->
    nksip_lib:hash({<<"refer">>, nksip_lib:to_binary(CSeqNum)});

make_id(#sipmsg{event={Event, Opts}}) ->
    Id = nksip_lib:get_value(<<"id">>, Opts),
    nksip_lib:hash({Event, Id}).


%% @private Finds a event.
-spec find(id()|nksip:request()|nksip:response(), nksip:dialog()) ->
    nksip:subscription() | not_found.

find(Id, #dialog{subscriptions=Subs}) when is_binary(Id) ->
    do_find(Id, Subs);

find(#sipmsg{}=Req, #dialog{subscriptions=Subs}) ->
    do_find(make_id(Req), Subs).

%% @private 
do_find(_, []) -> not_found;
do_find(Id, [#subscription{id=Id}=Subs|_]) -> Subs;
do_find(Id, [_|Rest]) -> do_find(Id, Rest).



%% @private Hack to find the UAS subscription from the UAC and the opposite way
remote_id(Handle, App) ->
    {_AppId0, SubsId, _DialogId, CallId} = parse_handle(Handle),
    {ok, DialogHandle} = nksip_dialog:get_handle(Handle),
    RemoteId = nksip_dialog_lib:remote_id(DialogHandle, App),
    {AppId1, RemDialogId, CallId} = nksip_dialog_lib:parse_handle(RemoteId),
    App1 = atom_to_binary(AppId1, latin1),
    <<$U, $_, SubsId/binary, $_, RemDialogId/binary, $_, App1/binary, $_, CallId/binary>>.


%% @private
-spec state(nksip:request()) ->
    nksip_subscription:subscription_state() | invalid.

state(#sipmsg{}=SipMsg) ->
    try
        case nksip_sipmsg:header(<<"subscription-state">>, SipMsg, tokens) of
            [{Name, Opts}] -> ok;
            _ -> Name = Opts = throw(invalid)
        end,
        case Name of
            <<"active">> -> 
                case nksip_lib:get_integer(<<"expires">>, Opts, -1) of
                    -1 -> Expires = undefined;
                    Expires when is_integer(Expires), Expires>=0 -> ok;
                    _ -> Expires = throw(invalid)
                end,
                 {active, Expires};
            <<"pending">> -> 
                case nksip_lib:get_integer(<<"expires">>, Opts, -1) of
                    -1 -> Expires = undefined;
                    Expires when is_integer(Expires), Expires>=0 -> ok;
                    _ -> Expires = throw(invalid)
                end,
                {pending, Expires};
            <<"terminated">> ->
                case nksip_lib:get_integer(<<"retry-after">>, Opts, -1) of
                    -1 -> Retry = undefined;
                    Retry when is_integer(Retry), Retry>=0 -> ok;
                    _ -> Retry = throw(invalid)
                end,
                case nksip_lib:get_value(<<"reason">>, Opts) of
                    undefined -> 
                        {terminated, undefined, undefined};
                    Reason0 ->
                        case catch binary_to_existing_atom(Reason0, latin1) of
                            {'EXIT', _} -> {terminated, undefined, undefined};
                            probation -> {terminated, probation, Retry};
                            giveup -> {terminated, giveup, Retry};
                            Reason -> {terminated, Reason, undefined}
                        end
                end;
            _ ->
                throw(invalid)
        end
    catch
        throw:invalid -> invalid
    end.



