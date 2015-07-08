
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

%% @private User Dialog Library Module.
-module(nksip_dialog_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([meta/2, metas/2, remote_meta/2, remote_metas/2]).
-export([get_handle/1, parse_handle/1, make_id/2, remote_id/2, change_app/2]).
-export_type([id/0]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% SIP Dialog unique ID
-type id() :: binary().


%% ===================================================================
%% Internal
%% ===================================================================



%% @doc Calculates a dialog's id.
-spec get_handle(nksip:dialog()|nksip:request()|nksip:response()|nksip:handle()) ->
    nksip:handle().

get_handle(#dialog{id=Id, app_id=AppId, call_id=CallId}) ->
    App = atom_to_binary(AppId, latin1),
    <<$D, $_, Id/binary, $_, App/binary, $_, CallId/binary>>;
get_handle(#sipmsg{dialog_id=DialogId, app_id=AppId, call_id=CallId}) ->
    App = atom_to_binary(AppId, latin1),
    <<$D, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>;
get_handle(<<"D_", _/binary>>=DialogId) ->
    DialogId;
get_handle(<<"U_", _/binary>>=Id) ->
    {AppId, _, DialogId, CallId} = nksip_subscription_lib:parse_handle(Id),
    App = atom_to_binary(AppId, latin1), 
    <<$D, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>;
get_handle(_) ->
    error(invalid_dialog).


%% @doc 
-spec parse_handle(nksip:handle()) -> 
    {nksip:app_id(), id(), nksip:call_id()}.

parse_handle(<<$D, $_, _/binary>>=Bin) ->
    <<$D, $_, Id:6/binary, $_, App:7/binary, $_, CallId/binary>> = Bin,
    {binary_to_existing_atom(App, latin1), Id, CallId};
parse_handle(_) ->
    error(invalid_handle).


%% @doc Get specific metadata from the dialog
-spec meta(nksip_dialog:field(), nksip:dialog()) -> 
    term().

meta(Field, #dialog{invite=I}=D) ->
    case Field of
        handle -> get_handle(D);
        internal_id -> D#dialog.id;
        app_id -> D#dialog.app_id;
        app_name -> apply(D#dialog.app_id, name, []);
        created -> D#dialog.created;
        updated -> D#dialog.updated;
        local_seq -> D#dialog.local_seq; 
        remote_seq  -> D#dialog.remote_seq; 
        local_uri -> D#dialog.local_uri;
        raw_local_uri -> nksip_unparse:uri(D#dialog.local_uri);
        remote_uri -> D#dialog.remote_uri;
        raw_remote_uri -> nksip_unparse:uri(D#dialog.remote_uri);
        local_target -> D#dialog.local_target;
        raw_local_target -> nksip_unparse:uri(D#dialog.local_target);
        remote_target -> D#dialog.remote_target;
        raw_remote_target -> nksip_unparse:uri(D#dialog.remote_target);
        early -> D#dialog.early;
        secure -> D#dialog.secure;
        route_set -> D#dialog.route_set;
        raw_route_set -> [nksip_lib:to_binary(Route) || Route <- D#dialog.route_set];
        invite_status when is_record(I, invite) -> I#invite.status;
        invite_status -> undefined;
        invite_answered when is_record(I, invite) -> I#invite.answered;
        invite_answered -> undefined;
        invite_local_sdp when is_record(I, invite) -> I#invite.local_sdp;
        invite_local_sdp -> undefined;
        invite_remote_sdp when is_record(I, invite) -> I#invite.remote_sdp;
        invite_remote_sdp -> undefined;
        invite_timeout when is_record(I, invite) -> read_timer(I#invite.timeout_timer);
        invite_timeout -> undefined;
        subscriptions -> 
            [nksip_subscription_lib:get_handle({user_subs, S, D}) || S <- D#dialog.subscriptions];
        call_id -> D#dialog.call_id;
        from_tag -> nksip_lib:get_binary(<<"tag">>, (D#dialog.local_uri)#uri.ext_opts);
        to_tag -> nksip_lib:get_binary(<<"tag">>, (D#dialog.remote_uri)#uri.ext_opts);
        full_dialog -> D;
        {function, Fun} -> Fun(D);
        _ -> error({invalid_field, Field}) 
    end.

%% @doc Get specific metadata from the dialog
-spec metas([nksip_dialog:field()], nksip:dialog()|nksip:handle()) -> 
    [{nksip_dialog:field(), term()}].

metas(Fields, #dialog{}=Dialog) when is_list(Fields) ->
    [{Field, meta(Field, Dialog)} || Field <- Fields].


%% @doc Extracts remote meta
-spec remote_meta(nksip_dialog:field(), nksip:handle()) ->
    {ok, term()} | {error, term()}.

remote_meta(Field, Handle) ->
    case remote_metas([Field], Handle) of
        {ok, [{_, Value}]} -> {ok, Value};
        {error, Error} -> {error, Error}
    end.


%% @doc Extracts remote metas
-spec remote_metas([nksip_dialog:field()], nksip:handle()) ->
    {ok, [{nksip_dialog:field(), term()}]} | {error, term()}.

remote_metas(Fields, Handle) when is_list(Fields) ->
    {AppId, DialogId, CallId} = parse_handle(Handle),
    Fun = fun(Dialog) ->
        case catch metas(Fields, Dialog) of
            {'EXIT', {{invalid_field, Field}, _}} -> 
                {error, {invalid_field, Field}};
            Values -> 
                {ok, Values}
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



%% @doc Calculates a <i>dialog's id</i> from a {@link nksip:request()} or
%% {@link nksip:response()} and a endpoint class.
%% Dialog ids are calculated as a hash over <i>Call-ID</i>, <i>From</i> tag 
%% and <i>To</i> Tag. Dialog ids with same From and To are different
%% for different endpoint classes.
-spec make_id(uac|uas, nksip:request()|nksip:response()) ->
    id().

make_id(Class, #sipmsg{from={_, FromTag}, to={_, ToTag}})
        when FromTag /= <<>>, ToTag /= <<>> ->
    make_id(Class, FromTag, ToTag);

make_id(Class, #sipmsg{from={_, FromTag}, to={_, <<>>}, class={req, Method}}=SipMsg)
    when FromTag /= <<>> andalso
         (Method=='INVITE' orelse Method=='REFER' orelse
          Method=='SUBSCRIBE' orelse Method=='NOTIFY') ->
    #sipmsg{to_tag_candidate=ToTag} = SipMsg,
    case ToTag of
        <<>> -> <<>>;
        _ -> make_id(Class, FromTag, ToTag)
    end;

make_id(_, #sipmsg{}) ->
    <<>>.


%% @private
-spec make_id(uac|uas, nksip:tag(), nksip:tag()) ->
    id().

make_id(Class, FromTag, ToTag) ->
    case Class of
        uac -> nksip_lib:hash({ToTag, FromTag});
        uas -> nksip_lib:hash({FromTag, ToTag})
    end.


%% @private Hack to find the UAS dialog from the UAC and the opposite way
remote_id(<<$D, _/binary>>=DialogId, App) ->
    {ok, AppId} = nksip:find_app_id(App),
    {ok, [{internal_id, BaseId}, {local_uri, LUri}, {remote_uri, RUri}, {call_id, CallId}]} =  
        nksip_dialog:metas([internal_id, local_uri, remote_uri, call_id], DialogId),
    FromTag = nksip_lib:get_binary(<<"tag">>, LUri#uri.ext_opts),
    ToTag = nksip_lib:get_binary(<<"tag">>, RUri#uri.ext_opts),
    Id = case make_id(uac, FromTag, ToTag) of
        BaseId -> make_id(uas, FromTag, ToTag);
        RemoteId -> RemoteId
    end,
    BinApp = atom_to_binary(AppId, latin1),
    <<$D, $_, Id/binary, $_, BinApp/binary, $_, CallId/binary>>.


%% @private Hack to find de dialog at another app in the same machine
change_app(Id, App) ->
    {_, DialogId, CallId} = parse_handle(Id),
    {ok, AppId1} = nksip:find_app_id(App),
    App1 = atom_to_binary(AppId1, latin1),
    <<$D, $_, DialogId/binary, $_, App1/binary, $_, CallId/binary>>.


%% @private
read_timer(Ref) when is_reference(Ref) -> (erlang:read_timer(Ref))/1000;
read_timer(_) -> undefined.




