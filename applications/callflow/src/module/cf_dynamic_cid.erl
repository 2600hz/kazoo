%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "action": "manual" | "list"
%%%   "media_id":"id_of_media"
%%%   "id":"{LIST_ID}" // the list referenced above is kept in a couchdb document with this id
%%%                    // required for  action:"list"
%%%   // optional after this
%%%   "interdigit_timeout":2000
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   William Lloyd
%%%-------------------------------------------------------------------
-module(cf_dynamic_cid).

-include("../callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-define(CONFIG_BIN(Key, Default)
        ,whapps_config:get_binary(?MOD_CONFIG_CAT, Key, Default)
       ).
-define(CONFIG_INT(Key, Default)
        ,whapps_config:get_integer(?MOD_CONFIG_CAT, Key, Default)
       ).

-record(prompts
        ,{accept_tone =
              ?CONFIG_BIN(<<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>)
          ,reject_tone =
              wh_media_util:get_prompt(
                  ?CONFIG_BIN(<<"reject_prompt">>, <<"dynamic-cid-invalid_using_default">>)
              )
          ,default_prompt =
              wh_media_util:get_prompt(
                  ?CONFIG_BIN(<<"default_prompt">>, <<"dynamic-cid-enter_cid">>)
              )
         }
       ).
-type prompts() :: #prompts{}.

-record(dynamic_cid
        ,{prompts = #prompts{} :: prompts()
          ,max_digits = ?CONFIG_INT(<<"max_digits">>, 10) :: integer()
          ,min_digits = ?CONFIG_INT(<<"min_digits">>, 10) :: integer()
          ,whitelist = ?CONFIG_BIN(<<"whitelist_regex">>, <<"\\d+">>) :: ne_binary()
         }
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case wh_json:get_value(<<"action">>, Data) of
        <<"list">> ->
            lager:info("user is choosing a caller id for this call from couchdb doc"),
            handle_list(Data, Call);
        _ ->
            lager:info("user must manually enter on keypad the caller id for this call"),
            handle_manual(Data, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle manual mode of dynamic cid
%% @end
%%--------------------------------------------------------------------
-spec handle_manual(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_manual(Data, Call) ->
    CID = collect_cid_number(Data, Call),

    Number = whapps_call:kvs_fetch('cf_capture_group', Call),

    Request = list_to_binary([Number, "@", whapps_call:request_realm(Call)]),
    To = list_to_binary([Number, "@", whapps_call:to_realm(Call)]),

    Updates = [{fun whapps_call:kvs_store/3, 'dynamic_cid', CID}
               ,{fun whapps_call:set_caller_id_number/2, CID}
               ,{fun whapps_call:set_request/2, Request}
               ,{fun whapps_call:set_to/2, To}
               ,{fun whapps_call:set_callee_id_number/2, Number}
              ],
    {'ok', C1} = cf_exe:get_call(Call),
    lager:info("setting the caller id number to ~s", [CID]),
    cf_exe:set_call(whapps_call:exec(Updates, C1)),

    lager:info("send the call onto real destination of: ~s", [Number]),
    cf_exe:continue(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc Read CID info from a list of CID defined in database
%% @end
%%--------------------------------------------------------------------
-spec handle_list(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_list(Data, Call) ->
    CallerIdNumber = whapps_call:caller_id_number(Call),
    lager:debug("callerid number before this module: ~s ", [CallerIdNumber]),

    {NewCidInfo, Dest} = get_list_entry(Data, Call),

    NewCallerIdNumber = wh_json:get_value(<<"number">>, NewCidInfo),
    NewCallerIdName = wh_json:get_value(<<"name">>, NewCidInfo),

    lager:info("setting the caller id number to ~s", [NewCallerIdNumber]),

    Updates = [{fun whapps_call:kvs_store/3, 'dynamic_cid', NewCallerIdNumber}
               ,{fun whapps_call:set_caller_id_number/2, NewCallerIdNumber}
               ,{fun whapps_call:set_caller_id_name/2, NewCallerIdName}
              ],

    cf_exe:set_call(whapps_call:exec(Updates, Call)),

    lager:debug("destination number from cf_capture_group regex: ~s ", [Dest]),
    Number = wnm_util:to_e164(Dest),
    lager:info("send the call onto real destination of: ~s", [Number]),

    maybe_route_to_callflow(Data, Call, Number).

-spec maybe_route_to_callflow(wh_json:object(), whapps_call:call(), ne_binary()) -> 'ok'.
maybe_route_to_callflow(Data, Call, Number) ->
    case cf_util:lookup_callflow(Number, whapps_call:account_id(Call)) of
        {'ok', Flow, 'true'} ->
            lager:info("callflow ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)]),
            Updates = [{fun whapps_call:set_request/2
                        ,list_to_binary([Number, "@", whapps_call:request_realm(Call)])
                       }
                       ,{fun whapps_call:set_to/2
                         ,list_to_binary([Number, "@", whapps_call:to_realm(Call)])
                        }
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(whapps_call:exec(Updates, C)),
            maybe_restrict_call(Data, Call, Number, Flow);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_restrict_call(wh_json:object(), whapps_call:call(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_restrict_call(Data, Call, Number, Flow) ->
    case should_restrict_call(Data, Call, Number) of
        'true' ->
            lager:info("Not allowed to call this destination, terminate", []),
            _ = whapps_call_command:answer(Call),
            _ = whapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = whapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            cf_exe:branch(wh_json:get_value(<<"flow">>, Flow), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_restrict_call(wh_json:object(), whapps_call:call(), ne_binary()) ->
                                  boolean().
should_restrict_call(Data, Call, Number) ->
    case wh_json:is_true(<<"enforce_call_restriction">>, Data, 'true') of
        'true' -> should_restrict_call(Call, Number);
        'false' ->
            lager:info("not enforcing call restrictions"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_restrict_call(whapps_call:call(), ne_binary()) -> boolean().
should_restrict_call(Call, Number) ->
    case  cf_endpoint:get(Call) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = wnm_util:classify_number(Number),
            lager:info("classified number as ~s", [Classification]),
            wh_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect CID number from user
%% @end
%%--------------------------------------------------------------------
-spec collect_cid_number(wh_json:object(), whapps_call:call()) -> ne_binary().
collect_cid_number(Data, Call) ->
    DynamicCID = #dynamic_cid{},
    Prompts = DynamicCID#dynamic_cid.prompts,
    _ = whapps_call_command:b_play(<<"silence_stream://100">>, Call),

    Media = case wh_json:get_ne_value(<<"media_id">>, Data) of
                'undefined' -> Prompts#prompts.default_prompt;
                Else -> Else
            end,

    Min = DynamicCID#dynamic_cid.min_digits,
    Max = DynamicCID#dynamic_cid.max_digits,
    Regex = DynamicCID#dynamic_cid.whitelist,
    DefaultCID = whapps_call:caller_id_number(Call),

    Interdigit = wh_json:get_integer_value(<<"interdigit_timeout">>
                                           ,Data
                                           ,whapps_call_command:default_interdigit_timeout()
                                          ),

    NoopId = whapps_call_command:play(Media, Call),

    CollectTimeout = whapps_call_command:default_collect_timeout(),
    case whapps_call_command:collect_digits(Max, CollectTimeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
            DefaultCID;
        {'ok', Digits} ->
            case re:run(Digits, Regex) of
                {'match', _} when byte_size(Digits) >= Min ->
                    whapps_call_command:play(Prompts#prompts.accept_tone, Call),
                    Digits;
                _ ->
                    _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                    DefaultCID
            end;
        {'error', _} ->
            _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
            DefaultCID
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull in document from database with the callerid switching information inside
%% @end
%%--------------------------------------------------------------------
-spec get_list_entry(wh_json:object(), whapps_call:call()) ->
                            {wh_json:object(), binary()} |
                            {'error', couch_mgr:couchbeam_error()}.
get_list_entry(Data, Call) ->
    ListId = wh_json:get_ne_value(<<"id">>, Data),
    AccountDb = whapps_call:account_db(Call),

    case couch_mgr:open_cache_doc(AccountDb, ListId) of
        {'ok', ListJObj} ->
            {CIDKey, DestNumber} = find_key_and_dest(ListJObj, Call),
            lager:debug("CIDKey ~p to lookup in match list document", [CIDKey]),
            NewCallerId = get_new_caller_id(CIDKey, ListJObj, Call),
            lager:info("new caller id: ~p",  [NewCallerId]),
            {NewCallerId, DestNumber};
        {'error', _Reason}=E ->
            lager:info("failed to load match list document ~s: ~p", [ListId, _Reason]),
            E
    end.

-spec find_key_and_dest(wh_json:object(), whapps_call:call()) -> {ne_binary(), ne_binary()}.
find_key_and_dest(ListJObj, Call) ->
    LengthDigits = wh_json:get_integer_value(<<"length">>, ListJObj),
    lager:debug("digit length to limit lookup key in number: ~p", [LengthDigits]),
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    <<CIDKey:LengthDigits/binary, Dest/binary>> = CaptureGroup,
    {CIDKey, Dest}.

-spec get_new_caller_id(integer(), wh_json:object(), whapps_call:call()) -> wh_json:object().
get_new_caller_id(CIDKey, ListJObj, Call) ->
    JObj = wh_json:get_ne_value(<<"entries">>, ListJObj, wh_json:new()),
    case wh_json:get_value(CIDKey, JObj) of
        'undefined' ->
            lager:debug("failed to find cid with key ~B, not changing cid", [CIDKey]),
            wh_json:from_list([{<<"number">>, whapps_call:caller_id_number(Call)}
                               ,{<<"name">>, whapps_call:caller_id_name(Call)}
                              ]);
        NewCallerId -> NewCallerId
    end.
