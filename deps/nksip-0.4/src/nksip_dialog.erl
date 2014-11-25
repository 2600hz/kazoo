
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

%% @doc User Dialog Management Module.
%% This module implements several utility functions related to dialogs.

-module(nksip_dialog).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([app_id/1, app_name/1, get_handle/1, call_id/1, meta/2, metas/2]).
-export([get_dialog/2, get_all/0, get_all/2, stop/1, bye_all/0, stop_all/0]).
-export([get_authorized_list/1, clear_authorized_list/1]).
-export([get_all_data/0]).
-export_type([invite_status/0, field/0, stop_reason/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% SIP Dialog stop reason
-type stop_reason() :: 
    nksip:sip_code() | caller_bye | callee_bye | forced |
    busy | cancelled | service_unavailable | declined | timeout |
    ack_timeout | no_events.

%% All the ways to specify a dialog
% -type spec() :: id() | nksip:handle().

-type field() :: 
    get_handle | app_id | app_name | created | updated | local_seq | remote_seq | 
    local_uri | raw_local_uri | remote_uri | raw_remote_uri | 
    local_target | raw_local_target | remote_target | raw_remote_target | 
    early | secure | route_set | raw_route_set | 
    invite_status | invite_answered | invite_local_sdp | invite_remote_sdp |
    invite_timeout | subscriptions | call_id | from_tag | to_tag.


%% All dialog INVITE states
-type invite_status() :: 
    init | proceeding_uac | proceeding_uas |accepted_uac | accepted_uas |
    confirmed | bye.



%% ===================================================================
%% Public
%% ===================================================================



%% @doc Calculates a dialog's id.
-spec get_handle(nksip:dialog()|nksip:request()|nksip:response()|nksip:handle()) ->
    {ok, nksip:handle()} | {error, term()}.

get_handle(<<Class, $_, _/binary>>=Handle) when Class==$R; Class==$S ->
    case nksip_sipmsg:remote_meta(dialog_handle, Handle) of
        {ok, DialogHandle} -> {ok, DialogHandle};
        {error, _} -> {error, invalid_dialog}
    end;
get_handle(Term) ->
    {ok, nksip_dialog_lib:get_handle(Term)}.


%% @doc Gets thel App of a dialog
-spec app_id(nksip:dialog()|nksip:handle()) ->
    {ok, nksip:app_id()}.

app_id(#dialog{app_id=AppId}) ->
    {ok, AppId};
app_id(Handle) ->
    {AppId, _DialogId, _CallId} = nksip_dialog_lib:parse_handle(Handle),
    {ok, AppId}.


%% @doc Gets app's name
-spec app_name(nksip:dialog()|nksip:handle()) -> 
    {ok, nksip:app_name()} | {error, term()}.

app_name(Term) -> 
    {ok, AppId} = app_id(Term),
    {ok, AppId:name()}.


%% @doc Gets thel Call-ID of a dialog
-spec call_id(nksip:dialog()|nksip:handle()) ->
    {ok, nksip:call_id()}.

call_id(#dialog{call_id=CallId}) ->
    {ok, CallId};
call_id(Handle) ->
    {_AppId, _DialogId, CallId} = nksip_dialog_lib:parse_handle(Handle),
    {ok, CallId}.


%% @doc Get a specific metadata
-spec meta(field(), nksip:dialog()|nksip:handle()) ->
    {ok, term()} | {error, term()}.

meta(Field, #dialog{}=Dialog) -> 
    {ok, nksip_dialog_lib:meta(Field, Dialog)};
meta(Field, <<Class, $_, _/binary>>=MsgHandle) when Class==$R; Class==$S ->
    case get_handle(MsgHandle) of
        {ok, DlgHandle} -> meta(Field, DlgHandle);
        {error, Error} -> {error, Error}
    end;
meta(Field, Handle) ->
    nksip_dialog_lib:remote_meta(Field, Handle).


%% @doc Get a group of specific metadata
-spec metas([field()], nksip:dialog()|nksip:handle()) ->
    {ok, [{field(), term()}]} | {error, term()}.

metas(Fields, #dialog{}=Dialog) when is_list(Fields) ->
    {ok, nksip_dialog_lib:metas(Fields, Dialog)};
metas(Fields, <<Class, $_, _/binary>>=MsgHandle) when Class==$R; Class==$S ->
    case get_handle(MsgHandle) of
        {ok, DlgHandle} -> metas(Fields, DlgHandle);
        {error, Error} -> {error, Error}
    end;
metas(Fields, Handle) when is_list(Fields) ->
    nksip_dialog_lib:remote_metas(Fields, Handle).



%% @doc Gets the dialog object corresponding to a request or subscription and a call
-spec get_dialog(nksip:request()|nksip:response()|nksip:subscription(), nksip:call()) ->
    {ok, nksip:dialog()} | {error, term()}.

get_dialog(#sipmsg{dialog_id=DialogId}, #call{}=Call) ->
    case nksip_call_dialog:find(DialogId, Call) of
        not_found -> {error, invalid_dialog};
        Dialog -> {ok, Dialog}
    end;

get_dialog({uses_subs, _Subs, Dialog}, _) ->
    {ok, Dialog}.


%% @doc Gets all started dialogs handles
-spec get_all() ->
    [nksip:handle()].

get_all() ->
    nksip_call:get_all_dialogs().


%% @doc Finds all started dialogs handles having `Call-ID'.
-spec get_all(nksip:app_name()|nksip:app_id(), nksip:call_id()) ->
    [nksip:handle()].

get_all(App, CallId) ->
    case nksip:find_app_id(App) of
        {ok, AppId} -> 
            case nksip_call:get_all_dialogs(AppId, CallId) of
                {ok, Handles} -> Handles;
                {error, _} -> []
            end;
        _ ->
            []
    end.


%% @doc Sends an in-dialog BYE to all existing dialogs.
-spec bye_all() ->
    ok.

bye_all() ->
    Fun = fun(DialogId) -> nksip_uac:bye(DialogId, [async]) end,
    lists:foreach(Fun, get_all()).


%% @doc Stops an existing dialog (remove it from memory).
-spec stop(nksip:handle()) ->
    ok | {error, term()}.

stop(Handle) ->
    {AppId, DialogId, CallId} = nksip_dialog_lib:parse_handle(Handle),
    nksip_call:stop_dialog(AppId, CallId, DialogId).


%% @doc Stops (deletes) all current dialogs.
-spec stop_all() ->
    ok.

stop_all() ->
    lists:foreach(fun(DialogId) -> stop(DialogId) end, get_all()).


%% @doc Gets the authorized list of transport, ip and ports for a dialog.
-spec get_authorized_list(nksip:handle()) ->
    [{nksip:protocol(), inet:ip_address(), inet:port_number()}].

get_authorized_list(Handle) ->
    {AppId, DialogId, CallId} = nksip_dialog_lib:parse_handle(Handle),
    case nksip_call:get_authorized_list(AppId, CallId, DialogId)of
        {ok, List} -> List;
        _ -> []
    end.


%% @doc Clears the authorized list of transport, ip and ports for a dialog.
-spec clear_authorized_list(nksip:handle()) ->
    ok | {error, term()}.

clear_authorized_list(Handle) ->
    {AppId, DialogId, CallId} = nksip_dialog_lib:parse_handle(Handle),
    nksip_call:clear_authorized_list(AppId, CallId, DialogId).



%% ===================================================================
%% Private
%% ===================================================================


%% @private Dumps all dialog information
%% Do not use it with many active dialogs!!
-spec get_all_data() ->
    [{nksip:handle(), nksip:optslist()}].

get_all_data() ->
    Now = nksip_lib:timestamp(),
    Fun = fun(DialogId, Acc) ->
        case meta(full_dialog, DialogId) of
            {ok, #dialog{}=Dialog} ->
                Data = {DialogId, [
                    {id, Dialog#dialog.id},
                    {app_id, Dialog#dialog.app_id},
                    {call_id, Dialog#dialog.call_id},
                    {local_uri, nksip_unparse:uri(Dialog#dialog.local_uri)},
                    {remote_uri, nksip_unparse:uri(Dialog#dialog.remote_uri)},
                    {created, Dialog#dialog.created},
                    {elapsed, Now - Dialog#dialog.created},
                    {updated, Dialog#dialog.updated},
                    {local_seq, Dialog#dialog.local_seq},
                    {remote_seq, Dialog#dialog.remote_seq},
                    {local_target, nksip_unparse:uri(Dialog#dialog.local_target)},
                    {remote_target, nksip_unparse:uri(Dialog#dialog.remote_target)},
                    {route_set, 
                        [nksip_unparse:uri(Route) || Route <- Dialog#dialog.route_set]},
                    {early, Dialog#dialog.early},
                    {invite, 
                        case Dialog#dialog.invite of
                            #invite{
                                status = InvStatus, 
                                local_sdp = LSDP, 
                                remote_sdp = RSDP,
                                timeout_timer  = Timeout
                            } -> 
                                Time = erlang:read_timer(Timeout)/1000,
                                {InvStatus, Time, LSDP, RSDP};
                            undefined ->
                                undefined
                        end},
                   {subscriptions,
                        [Id || #subscription{id=Id} <- Dialog#dialog.subscriptions]}
                ]},
                [Data|Acc];
            _ ->
                Acc
        end
    end,
    lists:foldl(Fun, [], get_all()).


