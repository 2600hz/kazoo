%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_util).

-include("blackhole.hrl").

-export([is_authorized/1]).
-export([respond_with_error/1, respond_with_error/3, respond_with_authn_failure/1]).
-export([get_callback_module/1]).
-export([send_error_message/3]).
-export([handle_event/3]).
-export([respond_with_success/1, respond_with_success/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(bh_context:context()) -> boolean().
is_authorized(Context) ->
    AuthEvent = <<"blackhole.authenticate">>,
    case blackhole_bindings:succeeded(blackhole_bindings:map(AuthEvent, Context)) of
        [] ->
            lager:debug("failed to authenticate"),
            'false';
        ['true'|_] ->
            lager:debug("is_authentic: true"),
            'true';
        [{'true', _}|_] ->
            lager:debug("is_authentic: true"),
            'true';
        [{'halt', _}|_] ->
            lager:debug("is_authentic: halt"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec respond_with_error(bh_context:context()) -> bh_context:context().
-spec respond_with_error(bh_context:context(), api_binary(), kz_json:object()) -> bh_context:context().
respond_with_error(Context) ->
    respond_with_error(Context, <<"error">>, kz_json:from_list([{<<"message">>, <<"unknown error">>}])).

respond_with_error(Context, Error, JObj) ->
    lager:debug(
      "Error: ~p for socket: ~s"
               ,[kz_json:get_value(<<"message">>, JObj), bh_context:websocket_session_id(Context)]
     ),
    WsPid = bh_context:websocket_pid(Context),
    blackhole_data_emitter:emit(WsPid, Error, JObj),
    Context.

-spec respond_with_success(bh_context:context()) -> bh_context:context().
respond_with_success(Context) ->
    Message = kz_json:from_list([{<<"message">>, <<"operation completed successfully">>}]),
    WsPid = bh_context:websocket_pid(Context),
    blackhole_data_emitter:emit(WsPid, <<"success">>, Message),
    Context.

-spec respond_with_success(bh_context:context(), ne_binary(), ne_binary()) -> bh_context:context().
respond_with_success(Context, Operation, Details) ->
    Message = kz_json:from_list([
                                {<<"operation">>, Operation}
                                ,{<<"detail">>, Details}
                                ]),
    WsPid = bh_context:websocket_pid(Context),
    blackhole_data_emitter:emit(WsPid, <<"success">>, Message),
    Context.


-spec error_module(atom()) -> ne_binary().
error_module(Module) ->
    ModuleName = erlang:atom_to_binary(Module, 'utf8'),
    <<"Error in module ", ModuleName/binary>>.

-spec send_error_message(bh_context:context(), atom() | ne_binary(), ne_binary()) -> bh_context:context().
send_error_message(Context, Module, ErrCause) when is_atom(Module) ->
    send_error_message(Context, error_module(Module), ErrCause);
send_error_message(Context, ErrMsg, ErrCause) ->
    ErrorJObj = kz_json:from_list([{<<"message">>, ErrMsg}, {<<"cause">>, ErrCause}]),
    respond_with_error(Context, <<"error">>, ErrorJObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec respond_with_authn_failure(bh_context:context()) -> bh_context:context().
respond_with_authn_failure(Context) ->
    Token = bh_context:auth_token(Context),
    lager:debug("authn failure: token ~s", [Token]),
    Error = kz_json:from_list([{<<"message">>, <<"invalid auth token">>}, {<<"cause">>, Token}]),
    respond_with_error(Context, <<"auth_failure">>, Error).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_callback_module(ne_binary()) -> atom().
get_callback_module(Binding) ->
    case binary:split(Binding, <<".">>) of
        [M|_] ->
            Mod = special_bindings(M),
            try kz_util:to_atom(<<"bh_", Mod/binary>>, 'true')
            catch
                'error':'badarg' -> 'undefined'
            end;
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec special_bindings(ne_binary()) -> ne_binary().
special_bindings(<<"doc_edited">>) -> <<"object">>;
special_bindings(<<"doc_created">>) -> <<"object">>;
special_bindings(<<"doc_deleted">>) -> <<"object">>;
special_bindings(M) -> M.

-spec handle_event(bh_context:context(), kz_json:object(), ne_binary()) -> 'ok'.
handle_event(#bh_context{binding=Binding} = Context, EventJObj, EventName) ->
    lager:debug("event:~s account_id:~s websocket:~s", [EventName
                                                       ,bh_context:account_id(Context)
                                                       ,bh_context:websocket_session_id(Context)]),
    kz_util:put_callid(EventJObj),
    NormJObj = kz_json:normalize_jobj(kz_json:set_value(<<"Binding">>, Binding, EventJObj)),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), EventName, NormJObj).
