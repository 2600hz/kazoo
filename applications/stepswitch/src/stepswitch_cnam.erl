%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc Lookup cnam
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_cnam).
-behaviour(gen_server).

-export([start_link/1]).
-export([render/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([lookup/1
        ,flush/0
        ]).

-include("stepswitch.hrl").

-type state() :: atom().

-define(SERVER, ?MODULE).

-define(DEFAULT_EXPIRES, 900).
-define(DEFAULT_PROVIDER, <<"opencnam">>).

-define(DISABLE_NORMALIZE
       ,kapps_config:get_is_true(?CNAM_CONFIG_CAT, <<"disable_normalize">>, 'false')
       ).

-define(CACHE_KEY(Number), {'cnam', Number}).

-define(CNAM_EXPIRES,
        kapps_config:get_integer(?CNAM_CONFIG_CAT, <<"cnam_expires">>, ?DEFAULT_EXPIRES)).
-define(CNAM_PROVIDER_MODULE,
        kz_term:to_atom(<<"stepswitch_cnam_", (kapps_config:get_binary(?CNAM_CONFIG_CAT, <<"provider">>, ?DEFAULT_PROVIDER))/binary>>, 'true')).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(any()) -> kz_types:startlink_ret().
start_link(_) ->
    _ = ssl:start(),
    gen_server:start_link(?SERVER, [], []).

-spec render(kz_json:object(), kz_term:ne_binary()) -> {'ok', iolist()} |
                                                       {'error', 'timeout'}.
render(JObj, Template) ->
    case catch poolboy:checkout(?STEPSWITCH_CNAM_POOL, 'false', 1000) of
        W when is_pid(W) ->
            Props = stepswitch_util:json_to_template_props(JObj),
            Reply = gen_server:call(W, {'render', Props, Template}),
            poolboy:checkin(?STEPSWITCH_CNAM_POOL, W),
            Reply;
        _Else -> {'error', 'timeout'}
    end.

-spec lookup(kz_json:object() | kz_term:ne_binary()) -> kz_json:object().
lookup(<<_/binary>> = Number) ->
    Num = case ?DISABLE_NORMALIZE of
              'false' -> knm_converters:normalize(Number);
              'true'  -> Number
          end,
    lookup(kz_json:set_values([{<<"phone_number">>, kz_util:uri_encode(Num)}
                              ,{<<"Caller-ID-Number">>, Num}
                              ]
                             ,kz_json:new()
                             )
          );
lookup(JObj) ->
    Number = kz_json:get_value(<<"Caller-ID-Number">>, JObj,  kz_privacy:anonymous_caller_id_number()),
    Num = case ?DISABLE_NORMALIZE of
              'false' -> knm_converters:normalize(Number);
              'true'  -> Number
          end,
    case kz_cache:fetch_local(?CACHE_NAME, cache_key(Num)) of
        {'ok', CNAM} ->
            update_request(JObj, CNAM, 'true');
        {'error', 'not_found'} ->
            CNAM = fetch_cnam(Num, set_phone_number(Num, JObj)),
            update_request(JObj, CNAM, 'false')
    end.

-spec set_phone_number(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_phone_number(Num, JObj) ->
    kz_json:set_value(<<"phone_number">>, kz_util:uri_encode(Num), JObj).

-spec update_request(kz_json:object(), kz_term:api_binary(), boolean()) -> kz_json:object().
update_request(JObj, 'undefined', _) -> JObj;
update_request(JObj, CNAM, FromCache) ->
    Props = [{<<"Caller-ID-Name">>, CNAM}
            ,{[<<"Custom-Channel-Vars">>, <<"Caller-ID-Name">>], CNAM}
            ,{[<<"Custom-Channel-Vars">>, <<"CNAM-From-Cache">>], FromCache}
            ],
    kz_json:set_values(Props, JObj).

-spec flush() -> non_neg_integer().
flush() ->
    kz_cache:filter_erase_local(?CACHE_NAME, fun flush_entries/2).

-spec flush_entries(any(), any()) -> boolean().
flush_entries(?CACHE_KEY(_), _) -> 'true';
flush_entries(_, _) -> 'false'.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    TemplateName = kz_term:to_atom(kz_datamgr:get_uuid(), 'true'),
    {'ok', TemplateName}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'render', Props, Template}, _, TemplateName) ->
    {'ok', TemplateName} = kz_template:compile(Template, TemplateName),
    {'ok', Result} = kz_template:render(TemplateName, Props),
    {'reply', {'ok', Result}, TemplateName};
handle_call(_Request, _From, TemplateName) ->
    {'reply', {'error', 'not_implemented'}, TemplateName}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, TemplateName) ->
    {'noreply', TemplateName}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, TemplateName) ->
    {'noreply', TemplateName}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _TemplateName) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _TemplateName) ->
    lager:debug("stepswitch cnam worker terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, TemplateName, _Extra) ->
    {'ok', TemplateName}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cache_key(kz_term:ne_binary()) -> {'cnam', kz_term:ne_binary()}.
cache_key(Number) -> ?CACHE_KEY(Number).

-spec fetch_cnam(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
fetch_cnam(Number, JObj) ->
    case make_request(Number, JObj) of
        'undefined' -> 'undefined';
        CNAM ->
            CacheProps = [{'expires', ?CNAM_EXPIRES}],
            kz_cache:store_local(?CACHE_NAME, cache_key(Number), CNAM, CacheProps),
            CNAM
    end.

-spec make_request(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
make_request(Number, JObj) ->
    Timeout = 2.99 * ?MILLISECONDS_IN_SECOND,
    case kz_util:runs_in(Timeout, fun request/2, [Number, JObj]) of
        {ok, CNAM} -> CNAM;
        timeout -> undefined
    end.

-spec request(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
request(Number, JObj) ->
    Mod = ?CNAM_PROVIDER_MODULE,
    Mod:request(Number, JObj).
