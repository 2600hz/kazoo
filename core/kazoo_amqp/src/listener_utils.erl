%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Utility functions for AMQP listeners to use to add/remove responders
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(listener_utils).

-export([add_responder/3
        ,rm_responder/3
        ,responder/1
        ,responder_mfa/2
        ]).

-include("listener_types.hrl").

-define(DEFAULT_CALLBACK, 'handle_req').

-export_type([responder/0
             ,responders/0
             ]).

-spec add_responder(responders(), responder_callback(), kz_json:json_proplist()) -> responders().
add_responder(Responders, Responder, Keys) when is_atom(Responder) ->
    add_responder(Responders, {Responder, ?DEFAULT_CALLBACK}, Keys);
add_responder(Responders, Responder, Keys) ->
    _ = maybe_init_responder(Responder, is_responder_known(Responders, Responder)),
    case responder(Responder) of
        'undefined' -> Responders;
        ResponderMFA -> lists:foldr(fun maybe_add_mapping/2, Responders, [{Evt, ResponderMFA} || Evt <- Keys])
    end.

-spec rm_responder(responders(), responder_callback(), kz_json:json_proplist()) -> responders().
%% remove all events for responder
rm_responder(Responders, Responder, Keys) when is_atom(Responder) ->
    rm_responder(Responders, {Responder, ?DEFAULT_CALLBACK}, Keys);
rm_responder(Responders, Responder, []) ->
    [ N || {_, Module}=N <- Responders, Module =/= Responder];
%% remove events in Keys for module Responder
rm_responder(Responders, Responder, Keys) ->
    %% if Evt is in the list of Keys and Module =:= Responder, remove it from the list of Responders
    [ N || {Evt, Module}=N <- Responders,
           (not (Module =:= Responder
                 andalso lists:member(Evt, Keys)
                )
           )
    ].

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec is_responder_known(responders(), responder_callback()) -> boolean().
is_responder_known(Responders, {Responder,_}=Callback) ->
    _ = maybe_load_responder(Responder),
    erlang:function_exported(Responder, 'init', 0)
        andalso kz_term:is_false(lists:keyfind(Callback, 2, Responders));
is_responder_known(_Responders, Callback)
  when is_function(Callback) -> 'false'.

maybe_load_responder(Responder) ->
    case erlang:module_loaded(Responder) of
        'true' -> 'ok';
        'false' ->
            case code:ensure_loaded(Responder) of
                {'module', Responder} -> 'ok';
                {'error', 'nofile'} ->
                    error({'error', 'no_responder_module', Responder})
            end
    end.

-spec maybe_add_mapping(responder(), responders()) -> responders().
maybe_add_mapping(Mapping, Acc) ->
    case lists:member(Mapping, Acc) of
        'true' -> Acc;
        'false' -> [Mapping | Acc]
    end.

-spec filter_responder_fun([tuple()], atom()) -> [tuple()].
filter_responder_fun(Exports, Fun) ->
    lists:filter(fun({ExportedFun, _}) -> ExportedFun =:= Fun end, Exports).

-spec responder_mfa(module(), atom()) -> mfa() | 'undefined'.
responder_mfa(Module, Fun) ->
    Exports = Module:module_info(exports),
    case filter_responder_fun(Exports, Fun) of
        [] -> 'undefined';
        [{_, I} | _] -> {Module, Fun, I}
    end.

-spec responder({module(), atom()} | responder_callback_fun()) -> responder() | 'undefined'.
responder({Module, Fun}) ->
    responder_mfa(Module, Fun);
responder(CallbackFun)
  when is_function(CallbackFun, 2);
       is_function(CallbackFun, 3);
       is_function(CallbackFun, 4) ->
    {'arity', Arity} = erlang:fun_info(CallbackFun,arity),
    {CallbackFun, Arity};
responder(_) -> 'undefined'.

-spec maybe_init_responder(responder_callback(), boolean()) -> 'ok'.
maybe_init_responder({Responder, _Fun}, 'true') when is_atom(Responder) ->
    try Responder:init() of
        _Init ->
            lager:debug("responder ~s init: ~p", [Responder, _Init])
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("responder ~s crashed: ~s: ~p", [Responder, _E, _R]),
            kz_util:log_stacktrace(ST)
    end;
maybe_init_responder(_, 'false') -> 'ok'.
