%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Helpers for manipulating the #bh_context{} record
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_context).

-export([new/0, new/2
        ,from_json/1, from_json/2
        ,to_json/1
        ,is_context/1
        ,is_authenticated/1
        ,is_superduper_admin/1
        ,add_error/2, add_error/3, errors/1
        ,success/1
        ]).

-export([setters/2
        ,auth_token/1, set_auth_token/2
        ,auth_account_id/1, set_auth_account_id/2
        ,bindings/1, set_bindings/2
        ,client_bindings/1
        ,bindings_from_json/1
        ,add_binding/2, add_bindings/2
        ,remove_binding/2, remove_bindings/2
        ,is_bound/2
        ,websocket_session_id/1, set_websocket_session_id/2
        ,websocket_pid/1, set_websocket_pid/2
        ,timestamp/1, set_timestamp/2
        ,name/1, set_name/2
        ,metadata/1, set_metadata/2
        ,destination/1, set_destination/2
        ,source/1, set_source/2
        ,req_id/1, set_req_id/2
        ,listeners/1, add_listeners/2, remove_listeners/2
        ,set_resp_data/2, set_resp_status/2
        ,resp_data/1, resp_status/1
        ]).

-export([match_auth_account_id/1, match_auth_account_id/2
        ,match_source/1, match_source/2

        ,id_position/0
        ]).

-include("blackhole.hrl").

%% {ClientBinding, AMQPBinding}
%% {<<"object.doc_created.user">>, <<"doc_created.{ACCOUNT_DB}.user.*">>}
-type binding() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-type bindings() :: [binding()].

-record(bh_context, {auth_token = <<>> :: kz_term:api_binary() | '_'
                    ,auth_account_id :: kz_term:api_binary() | '_'
                    ,bindings = [] :: bindings() | '_'
                    ,websocket_session_id :: kz_term:api_binary() | '_'
                    ,websocket_pid :: kz_term:api_pid() | '_'
                    ,req_id = kz_binary:rand_hex(16) :: kz_term:ne_binary() | '_'
                    ,timestamp = kz_time:now_s() :: kz_time:gregorian_seconds() | '_'
                    ,name :: kz_term:api_binary() | '_'
                    ,metadata :: any() | '_'
                    ,destination = kz_util:node_hostname() :: kz_term:ne_binary() | '_'
                    ,source :: kz_term:api_binary() | '_'
                    ,errors = [] :: kz_term:ne_binaries() | '_'
                    ,result = 'ok' :: 'ok' | 'error' | 'shutdown' | '_'
                    ,listeners = [] :: list() | '_'
                    ,resp_status = <<"success">> :: kz_term:ne_binary() | '_'
                    ,resp_data = kz_json:new() :: kz_json:object() | '_'
                    }).

-type context() :: #bh_context{}.
-type setter_fun_1() :: fun((context()) -> context()).
-type setter_fun_2() :: fun((context(), any()) -> context()).
-type setter_fun_3() :: fun((context(), any(), any()) -> context()).
-type setter_kv() :: setter_fun_1() |
                     {setter_fun_2(), any()} |
                     {setter_fun_3(), any(), any()}.
-type setters() :: [setter_kv()].

-export_type([context/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec new() -> context().
new()->
    #bh_context{}.

-spec new(pid(), kz_term:ne_binary()) -> context().
new(SessionPid, SessionId) ->
    Setters = [{fun set_websocket_session_id/2, SessionId}
              ,{fun set_websocket_pid/2, SessionPid}
              ],
    setters(new(), Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec from_json(kz_json:object()) -> context().
from_json(JObj) ->
    from_json(new(), JObj).

-spec from_json(context(), kz_json:object()) -> context().
from_json(Context, JObj) ->
    Rand = kz_binary:rand_hex(16),
    Setters = [{fun set_auth_token/2, kz_json:get_ne_binary_value(<<"auth_token">>, JObj)}
              ,{fun set_req_id/2, kz_json:get_ne_binary_value(<<"request_id">>, JObj, Rand)}
              ,{fun set_name/2, kz_json:get_value(<<"name">>, JObj)}
              ,{fun set_metadata/2, kz_json:get_value(<<"metadata">>, JObj)}
              ],
    setters(Context, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(context()) -> kz_json:object().
to_json(Context) ->
    kz_json:from_list(
      [{<<"auth_token">>, auth_token(Context)}
      ,{<<"auth_account_id">>, auth_account_id(Context)}
      ,{<<"bindings">>, [ClientBinding || {ClientBinding, _} <- bindings(Context)]}
      ,{<<"websocket_session_id">>, websocket_session_id(Context)}
      ,{<<"timestamp">>, timestamp(Context)}
      ,{<<"name">>, name(Context)}
      ,{<<"metadata">>, metadata(Context)}
      ,{<<"destination">>, destination(Context)}
      ,{<<"source">>, source(Context)}
      ,{<<"req_id">>, req_id(Context)}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_context(any()) -> boolean().
is_context(#bh_context{}) -> 'true';
is_context(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(context(), setters()) -> context().
setters(Context, Setters) ->
    lists:foldl(fun setters_fold/2, Context, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec auth_token(context()) -> kz_term:api_binary().
auth_token(#bh_context{auth_token=AuthToken}) ->
    AuthToken.

-spec set_auth_token(context(), kz_term:ne_binary()) -> context().
set_auth_token(#bh_context{}=Context, AuthToken) ->
    Context#bh_context{auth_token=AuthToken}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec auth_account_id(context()) -> kz_term:api_binary().
auth_account_id(#bh_context{auth_account_id=AuthBy}) ->
    AuthBy.

-spec set_auth_account_id(context(), kz_term:ne_binary()) -> context().
set_auth_account_id(#bh_context{}=Context, AuthBy) ->
    Context#bh_context{auth_account_id=AuthBy}.

-spec is_superduper_admin(context()) -> boolean().
is_superduper_admin(#bh_context{auth_account_id=AccountId}) ->
    kzd_accounts:is_superduper_admin(AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bindings(context()) -> bindings().
bindings(#bh_context{bindings=Bds}) -> Bds.

-spec bindings_from_json(kz_json:object()) -> kz_term:ne_binaries().
bindings_from_json(JObj) ->
    case kz_json:get_value(<<"binding">>, JObj) of
        'undefined' ->
            kz_json:get_value(<<"bindings">>, JObj, []);
        Binding -> [Binding]
    end.

-spec client_bindings(context()) -> kz_term:ne_binaries().
client_bindings(#bh_context{bindings=Bs}) ->
    [ClientBinding || {ClientBinding, _AMQPBinding} <- Bs].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bindings(context(), bindings()) -> context().
set_bindings(Context, Bindings) ->
    Context#bh_context{bindings=Bindings}.

-spec add_binding(context(), binding()) -> context().
add_binding(#bh_context{bindings=Bds}=Context, {_, _}=Binding) ->
    Context#bh_context{bindings=[Binding|Bds]}.

-spec add_bindings(context(), bindings()) -> context().
add_bindings(#bh_context{bindings=Bds}=Context, Bindings) ->
    Context#bh_context{bindings=Bds ++ Bindings}.

-spec remove_binding(context(), binding()) -> context().
remove_binding(#bh_context{bindings=Bds}=Context, {_,_}=Binding) ->
    Context#bh_context{bindings=lists:delete(Binding, Bds)}.

-spec remove_bindings(context(), bindings()) -> context().
remove_bindings(#bh_context{bindings=Bds}=Context, Bindings) ->
    Context#bh_context{bindings= Bds -- Bindings}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_bound(context(), binding()) -> boolean().
is_bound(#bh_context{bindings=Bds}, {_,_}=Binding) ->
    lists:member(Binding, Bds).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec websocket_pid(context()) -> pid().
websocket_pid(#bh_context{websocket_pid=SocketPid}) -> SocketPid.

-spec set_websocket_pid(context(), pid()) -> context().
set_websocket_pid(#bh_context{}=Context, SocketPid) ->
    Context#bh_context{websocket_pid=SocketPid}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec websocket_session_id(context()) -> kz_term:api_binary().
websocket_session_id(#bh_context{websocket_session_id=SessionId}) ->
    SessionId.

-spec set_websocket_session_id(context(), kz_term:ne_binary()) -> context().
set_websocket_session_id(#bh_context{}=Context, SessionId) ->
    Context#bh_context{websocket_session_id=SessionId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec timestamp(context()) -> kz_time:gregorian_seconds().
timestamp(#bh_context{timestamp=Timestamp}) ->
    Timestamp.

-spec set_timestamp(context(), kz_time:gregorian_seconds()) -> context().
set_timestamp(#bh_context{}=Context, Timestamp) ->
    Context#bh_context{timestamp=Timestamp}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(context()) -> kz_term:ne_binary().
name(#bh_context{name=Name}) ->
    Name.

-spec set_name(context(), kz_term:ne_binary()) -> context().
set_name(#bh_context{}=Context, Name) ->
    Context#bh_context{name=Name}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec metadata(context()) -> any().
metadata(#bh_context{metadata=Meta}) ->
    Meta.

-spec set_metadata(context(), any()) -> context().
set_metadata(#bh_context{}=Context, Meta) ->
    Context#bh_context{metadata=Meta}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec source(context()) -> kz_term:ne_binary().
source(#bh_context{source=Source}) ->
    Source.

-spec set_source(context(), kz_term:ne_binary()) -> context().
set_source(#bh_context{}=Context, Source) ->
    Context#bh_context{source=Source}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec destination(context()) -> kz_term:ne_binary().
destination(#bh_context{destination=Destination}) ->
    Destination.

-spec set_destination(context(), kz_term:ne_binary()) -> context().
set_destination(#bh_context{}=Context, Destination) ->
    Context#bh_context{destination=Destination}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec req_id(context()) -> kz_term:ne_binary().
req_id(#bh_context{req_id=Id}) ->
    Id.

-spec set_req_id(context(), kz_term:ne_binary()) -> context().
set_req_id(#bh_context{}=Context, ReqId) ->
    kz_log:put_callid(ReqId),
    Context#bh_context{req_id=ReqId}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters_fold(setter_kv(), context()) -> context().
setters_fold({F, V}, C) -> F(C, V);
setters_fold({F, K, V}, C) -> F(C, K, V);
setters_fold(F, C) when is_function(F, 1) -> F(C).

-spec is_authenticated(context()) -> boolean().
is_authenticated(#bh_context{auth_account_id='undefined'}) -> 'false';
is_authenticated(_) -> 'true'.

-spec add_error(context(), kz_term:text()) -> context().
add_error(Context, Error) ->
    add_error(Context, 'error', Error).

-spec add_error(context(), 'ok' | 'error' | 'shutdown', kz_term:text()) -> context().
add_error(#bh_context{errors=Errors}=Context, Result, Error) ->
    Context#bh_context{result=Result, errors=[kz_term:to_binary(Error) | Errors]}.

-spec errors(context()) -> kz_term:ne_binaries().
errors(#bh_context{errors=Errors}) -> Errors.

-spec add_listeners(context(), list()) -> context().
add_listeners(#bh_context{listeners=BListeners}=Context, Listeners) ->
    Context#bh_context{listeners = BListeners ++ Listeners}.

-spec remove_listeners(context(), list()) -> context().
remove_listeners(#bh_context{listeners=BListeners}=Context, Listeners) ->
    Context#bh_context{listeners = BListeners -- Listeners}.

-spec listeners(context()) -> list().
listeners(#bh_context{listeners=BListeners}) ->
    BListeners.

-spec success(context()) -> boolean().
success(#bh_context{errors=[]}) -> 'true';
success(#bh_context{}) -> 'false'.

-spec set_resp_data(context(), kz_json:object()) -> context().
set_resp_data(#bh_context{}=Context, Data) ->
    Context#bh_context{resp_data=Data}.

-spec set_resp_status(context(), kz_term:ne_binary()) -> context().
set_resp_status(#bh_context{}=Context, Status) ->
    Context#bh_context{resp_status=Status}.

-spec resp_data(context()) -> kz_json:object().
resp_data(#bh_context{resp_data=Data}) ->
    Data.

-spec resp_status(context()) -> kz_term:ne_binary().
resp_status(#bh_context{resp_status=Status}) ->
    Status.

-spec match_auth_account_id(kz_term:ne_binary()) -> context().
match_auth_account_id(<<AuthAccountId/binary>>) ->
    match_auth_account_id(AuthAccountId, #bh_context{_='_'}).

-spec match_auth_account_id(kz_term:ne_binary(), context()) -> context().
match_auth_account_id(<<AuthAccountId/binary>>, #bh_context{}=Context) ->
    Context#bh_context{auth_account_id=AuthAccountId}.

-spec match_source(kz_term:ne_binary()) -> context().
match_source(<<Source/binary>>) ->
    match_source(Source, #bh_context{_='_'}).

-spec match_source(kz_term:ne_binary(), context()) -> context().
match_source(<<Source/binary>>, #bh_context{}=Context) ->
    Context#bh_context{source=Source}.

-spec id_position() -> integer().
id_position() ->
    #bh_context.websocket_session_id.
