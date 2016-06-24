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

-include_lib("callflow/src/callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-define(CONFIG_BIN(Key, Default)
        ,kapps_config:get_binary(?MOD_CONFIG_CAT, Key, Default)
       ).
-define(CONFIG_INT(Key, Default)
        ,kapps_config:get_integer(?MOD_CONFIG_CAT, Key, Default)
       ).

-record(prompts
        ,{accept_tone =
              ?CONFIG_BIN(<<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>)
          ,reject_tone =
              kz_media_util:get_prompt(
                ?CONFIG_BIN(<<"reject_prompt">>, <<"dynamic-cid-invalid_using_default">>)
              )
          ,default_prompt =
              kz_media_util:get_prompt(
                ?CONFIG_BIN(<<"default_prompt">>, <<"dynamic-cid-enter_cid">>)
              )
         }).
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
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case kz_json:get_value(<<"action">>, Data) of
        <<"list">> ->
            lager:info("user is choosing a caller id for this call from couchdb doc"),
            handle_list(Data, Call);
        <<"lists">> ->
            lager:info("using account's lists/entries view to get new cid info"),
            handle_lists(Data, Call);
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
-spec handle_manual(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_manual(Data, Call) ->
    CID = collect_cid_number(Data, Call),

    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    Number = knm_converters:normalize(CaptureGroup),

    Request = list_to_binary([Number, "@", kapps_call:request_realm(Call)]),
    To = list_to_binary([Number, "@", kapps_call:to_realm(Call)]),

    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', CID}
               ,{fun kapps_call:set_caller_id_number/2, CID}
               ,{fun kapps_call:set_request/2, Request}
               ,{fun kapps_call:set_to/2, To}
               ,{fun kapps_call:set_callee_id_number/2, Number}
              ],
    {'ok', C1} = cf_exe:get_call(Call),
    lager:info("setting the caller id number to ~s", [CID]),
    cf_exe:set_call(kapps_call:exec(Updates, C1)),

    lager:info("send the call onto real destination of: ~s", [Number]),
    cf_exe:continue(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc Read CID info from a list of CID defined in database
%% @end
%%--------------------------------------------------------------------
-type cid_entry() :: {binary(), binary(), binary()} | {'error', kz_data:data_error()}.

-spec handle_list(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_list(Data, Call) ->
    maybe_procees_with_call(get_list_entry(Data, Call), Data, Call).

-spec handle_lists(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_lists(Data, Call) ->
    maybe_procees_with_call(get_lists_entry(Data, Call), Data, Call).

-spec maybe_procees_with_call(cid_entry(), kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_procees_with_call({<<>>, <<>>, _}, _, Call) ->
    lager:debug("empty cid entry, hanging up"),
    _ = kapps_call_command:answer(Call),
    _ = kapps_call_command:prompt(<<"menu-invalid_entry">>, Call),
    kapps_call_command:queued_hangup(Call);
maybe_procees_with_call({NewCallerIdName, NewCallerIdNumber, Dest}, Data, Call) ->
    proceed_with_call(NewCallerIdName, NewCallerIdNumber, Dest, Data, Call);
maybe_procees_with_call(_, _, Call) ->
    _ = kapps_call_command:answer(Call),
    _ = kapps_call_command:prompt(<<"fault-can_not_be_completed_at_this_time">>, Call),
    kapps_call_command:queued_hangup(Call).

-spec proceed_with_call(ne_binary(), ne_binary(), binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
proceed_with_call(NewCallerIdName, NewCallerIdNumber, Dest, Data, Call) ->
    lager:debug("caller id number is about to be changed from: ~p to: ~p ", [kapps_call:caller_id_number(Call), NewCallerIdNumber]),
    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', NewCallerIdNumber}
               ,{fun kapps_call:set_caller_id_number/2, NewCallerIdNumber}
               ,{fun kapps_call:set_caller_id_name/2, NewCallerIdName}
              ],
    cf_exe:set_call(kapps_call:exec(Updates, Call)),
    Number = knm_converters:normalize(Dest),
    lager:info("send the call onto real destination of: ~s", [Number]),
    maybe_route_to_callflow(Data, Call, Number).

-spec maybe_route_to_callflow(kz_json:object(), kapps_call:call(), ne_binary()) -> 'ok'.
maybe_route_to_callflow(Data, Call, Number) ->
    case cf_util:lookup_callflow(Number, kapps_call:account_id(Call)) of
        {'ok', Flow, 'true'} ->
            lager:info("callflow ~s satisfies request", [kz_json:get_value(<<"_id">>, Flow)]),
            Updates = [{fun kapps_call:set_request/2
                        ,list_to_binary([Number, "@", kapps_call:request_realm(Call)])
                       }
                       ,{fun kapps_call:set_to/2
                         ,list_to_binary([Number, "@", kapps_call:to_realm(Call)])
                        }
                      ],
            {'ok', C} = cf_exe:get_call(Call),
            cf_exe:set_call(kapps_call:exec(Updates, C)),
            maybe_restrict_call(Data, Call, Number, Flow);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = kapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_restrict_call(kz_json:object(), kapps_call:call(), ne_binary(), kz_json:object()) -> 'ok'.
maybe_restrict_call(Data, Call, Number, Flow) ->
    case should_restrict_call(Data, Call, Number) of
        'true' ->
            lager:info("Not allowed to call this destination, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            cf_exe:branch(kz_json:get_value(<<"flow">>, Flow), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_restrict_call(kz_json:object(), kapps_call:call(), ne_binary()) ->
                                  boolean().
should_restrict_call(Data, Call, Number) ->
    case kz_json:is_true(<<"enforce_call_restriction">>, Data, 'true') of
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
-spec should_restrict_call(kapps_call:call(), ne_binary()) -> boolean().
should_restrict_call(Call, Number) ->
    case  kz_endpoint:get(Call) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = knm_converters:classify(Number),
            lager:info("classified number as ~s", [Classification]),
            kz_json:get_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Collect CID number from user
%% @end
%%--------------------------------------------------------------------
-spec collect_cid_number(kz_json:object(), kapps_call:call()) -> ne_binary().
collect_cid_number(Data, Call) ->
    DynamicCID = #dynamic_cid{},
    Prompts = DynamicCID#dynamic_cid.prompts,
    _ = kapps_call_command:b_play(<<"silence_stream://100">>, Call),

    Media = case kz_json:get_ne_value(<<"media_id">>, Data) of
                'undefined' -> Prompts#prompts.default_prompt;
                Else -> Else
            end,

    Min = DynamicCID#dynamic_cid.min_digits,
    Max = DynamicCID#dynamic_cid.max_digits,
    Regex = DynamicCID#dynamic_cid.whitelist,
    DefaultCID = kapps_call:caller_id_number(Call),

    Interdigit = kz_json:get_integer_value(<<"interdigit_timeout">>
                                           ,Data
                                           ,kapps_call_command:default_interdigit_timeout()
                                          ),

    NoopId = kapps_call_command:play(Media, Call),

    CollectTimeout = kapps_call_command:default_collect_timeout(),
    case kapps_call_command:collect_digits(Max, CollectTimeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
            DefaultCID;
        {'ok', Digits} ->
            case re:run(Digits, Regex) of
                {'match', _} when byte_size(Digits) >= Min ->
                    kapps_call_command:play(Prompts#prompts.accept_tone, Call),
                    Digits;
                _ ->
                    _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
                    DefaultCID
            end;
        {'error', _} ->
            _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
            DefaultCID
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull in document from database with the callerid switching information inside
%% @end
%%--------------------------------------------------------------------
-spec get_list_entry(kz_json:object(), kapps_call:call()) -> cid_entry().
get_list_entry(Data, Call) ->
    ListId = kz_json:get_ne_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),

    case kz_datamgr:open_cache_doc(AccountDb, ListId) of
        {'ok', ListJObj} ->
            {CIDKey, DestNumber} = find_key_and_dest(ListJObj, Call),
            {NewCallerIdName, NewCallerIdNumber} = get_new_caller_id(CIDKey, ListJObj),
            {NewCallerIdName, NewCallerIdNumber, DestNumber};
        {'error', _Reason}=E ->
            lager:info("failed to load match list document ~s: ~p", [ListId, _Reason]),
            E
    end.

-spec find_key_and_dest(kz_json:object(), kapps_call:call()) -> {binary(), binary()}.
find_key_and_dest(ListJObj, Call) ->
    LengthDigits = kz_json:get_integer_value(<<"length">>, ListJObj),
    lager:debug("digit length to limit lookup key in number: ~p", [LengthDigits]),
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    <<CIDKey:LengthDigits/binary, Dest/binary>> = CaptureGroup,
    {CIDKey, Dest}.

-spec get_new_caller_id(binary(), kz_json:object()) -> {binary(), binary()}.
get_new_caller_id(CIDKey, ListJObj) ->

    JObj = kz_json:get_ne_value(<<"entries">>, ListJObj, kz_json:new()),
    case kz_json:get_value(CIDKey, JObj) of
        'undefined' -> {<<>>, <<>>};
        NewCallerId ->
            {kz_json:get_binary_value(<<"name">>, NewCallerId, <<>>)
             ,kz_json:get_binary_value(<<"number">>, NewCallerId, <<>>)}
    end.

-spec get_lists_entry(kz_json:object(), kapps_call:call()) -> cid_entry().
get_lists_entry(Data, Call) ->
    ListId = kz_json:get_ne_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb,<<"lists/entries">>,[{'key', ListId}]) of
        {'ok', Entries} ->
            CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
            <<CIDKey:2/binary, Dest/binary>> = CaptureGroup,
            {NewCallerIdName, NewCallerIdNumber} = cid_key_lookup(CIDKey, Entries),
            {NewCallerIdName, NewCallerIdNumber, Dest};
        {'error', Reason} = E ->
            lager:info("failed to load match list document ~s: ~p", [ListId, Reason]),
            E
    end.

-spec cid_key_lookup(binary(), kz_json:objects()) -> {binary(), binary()}.
cid_key_lookup(CIDKey, Entries) ->
    case lists:foldl(fun(Entry, Acc) -> cidkey_wanted(CIDKey, Entry, Acc) end, [], Entries) of
        [{NewCallerIdName, NewCallerIdNumber}|_] -> {NewCallerIdName, NewCallerIdNumber};
        _ -> {<<>>, <<>>}
    end.

-spec cidkey_wanted(binary(), kz_json:object(), proplist()) -> proplist().
cidkey_wanted(CIDKey, Entry, Acc) ->
    case kz_json:get_binary_value([<<"value">>, <<"cid_key">>], Entry) == CIDKey of
        'true' -> Acc ++ [{kz_json:get_binary_value([<<"value">>, <<"cid_name">>], Entry, <<>>)
                           ,kz_json:get_binary_value([<<"value">>, <<"cid_number">>], Entry, <<>>)
                          }];
        'false' -> Acc
    end.
