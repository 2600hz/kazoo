%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
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
-export([maybe_add_binding_to_listener/3
         ,maybe_rm_binding_from_listener/3
        ]).
-export([respond_with_error/1, respond_with_error/3, respond_with_authn_failure/1]).
-export([get_callback_module/1]).
-export([remove_binding/2]).

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
-spec maybe_add_binding_to_listener(atom(), ne_binary(), bh_context:context()) -> 'ok'.
maybe_add_binding_to_listener(Module, Binding, Context) ->
    try Module:add_amqp_binding(Binding, Context) of
        _Result -> 'ok'
    catch
        _ ->
            lager:debug("could not exec ~s:add_amqp_binding", [Module]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_rm_binding_from_listener(atom(), ne_binary(), bh_context:context()) -> 'ok'.
maybe_rm_binding_from_listener(Module, Binding, Context) ->
    lager:debug("remove some amqp bindings for module: ~s ~s", [Module, Binding]),
    try Module:rm_amqp_binding(Binding, Context) of
        _ -> 'ok'
    catch
        _ ->
            lager:debug("could not exec ~s:rm_amqp_binding", [Module]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec respond_with_error(bh_context:context()) -> bh_context:context().
-spec respond_with_error(bh_context:context(), maybe(binary()), kz_json:object()) -> bh_context:context().
respond_with_error(Context) ->
    respond_with_error(
        Context
        ,<<"error">>
        ,kz_json:from_list([
            {<<"message">>, <<"unknown error">>}
        ])
    ).

respond_with_error(Context, Error, JObj) ->
    lager:debug(
        "Error: ~p for socket: ~s"
        ,[kz_json:get_value(<<"message">>, JObj), bh_context:websocket_session_id(Context)]
    ),
    blackhole_data_emitter:emit(
        bh_context:websocket_pid(Context)
        ,Error
        ,JObj
    ),
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec respond_with_authn_failure(bh_context:context()) -> bh_context:context().
respond_with_authn_failure(Context) ->
    Token = bh_context:auth_token(Context),
    lager:debug("authn failure: token ~s", [Token]),
    respond_with_error(
        Context
        ,<<"auth_failure">>
        ,kz_json:from_list([
            {<<"message">>, <<"invalid auth token">>}
            ,{<<"cause">>, Token}
        ])
    ).

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
            try kz_util:to_atom(<<"bh_", Mod/binary>>, 'true') of
                Module -> Module
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_binding(ne_binary(), bh_context:context()) -> 'ok'.
remove_binding(Binding, Context) ->
    case ?MODULE:get_callback_module(Binding) of
        'undefined' -> 'ok';
        Module ->
            ?MODULE:maybe_rm_binding_from_listener(Module, Binding, Context),
            blackhole_bindings:unbind(Binding, Module, 'handle_event', Context)
    end.
