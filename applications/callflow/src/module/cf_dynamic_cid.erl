%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
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

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-record(prompts
       ,{accept_tone =
             kapps_config:get_binary(?MOD_CONFIG_CAT, <<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>)
        ,reject_tone =
             kz_media_util:get_prompt(
               kapps_config:get_binary(?MOD_CONFIG_CAT, <<"reject_prompt">>, <<"dynamic-cid-invalid_using_default">>)
              )
        ,default_prompt =
             kz_media_util:get_prompt(
               kapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_prompt">>, <<"dynamic-cid-enter_cid">>)
              )
        }).
-type prompts() :: #prompts{}.

-record(dynamic_cid
       ,{prompts = #prompts{} :: prompts()
        ,default_max_digits = kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_digits">>, 10) :: integer()
        ,default_min_digits = kapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_digits">>, 10) :: integer()
        ,default_whitelist = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"whitelist_regex">>, <<"\\d+">>) :: ne_binary()
        }
       ).

-type cid() :: {ne_binary(), ne_binary()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
-spec handle(kz_json:object(), kapps_call:call(), api_binary(), api_binary()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    Action = kz_json:get_value(<<"action">>, Data),
    handle(Data, Call, Action, CaptureGroup).

handle(Data, Call, <<"list">>, ?NE_BINARY = _CaptureGroup) ->
    lager:info("user is choosing a caller id for this call from couchdb doc"),
    handle_list(Data, Call);
handle(Data, Call, <<"lists">>, ?NE_BINARY = _CaptureGroup) ->
    lager:info("using account's lists/entries view to get new cid info"),
    handle_lists(Data, Call);
handle(Data, Call, <<"static">>, CaptureGroup) ->
    lager:info("user chose a static caller id for this call"),
    handle_static(Data, Call, CaptureGroup);
handle(Data, Call, _Manual, ?NE_BINARY = CaptureGroup) ->
    lager:info("user must manually enter on keypad the caller id for this call"),
    handle_manual(Data, Call, CaptureGroup);
handle(Data, Call, _Manual, CaptureGroup) ->
    lager:info("capture group is not present, forcing manual action. user must manually enter on keypad the caller id for this call"),
    handle_manual(Data, Call, CaptureGroup).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle manual mode of dynamic cid
%% @end
%%--------------------------------------------------------------------
-spec handle_manual(kz_json:object(), kapps_call:call(), api_binary()) -> 'ok'.
handle_manual(Data, Call, CaptureGroup) ->
    CID = collect_cid_number(Data, Call),
    update_call(Call, CID, CaptureGroup),
    kapps_call_command:flush_dtmf(Call),
    cf_exe:continue(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle static mode of dynamic cid
%% @end
%%--------------------------------------------------------------------
-spec handle_static(kz_json:object(), kapps_call:call(), api_binary()) -> 'ok'.
handle_static(Data, Call, CaptureGroup) ->
    {CIDName, CIDNumber} = get_static_cid_entry(Data, Call),
    update_call(Call, CIDNumber, CIDName, CaptureGroup),
    cf_exe:continue(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc Read CID info from a list of CID defined in database
%% @end
%%--------------------------------------------------------------------
-type list_cid_entry() :: {ne_binary(), ne_binary(), ne_binary()} | {'error', kz_data:data_error()}.

-spec handle_list(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_list(Data, Call) ->
    maybe_proceed_with_call(get_list_entry(Data, Call), Data, Call).

-spec handle_lists(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_lists(Data, Call) ->
    maybe_proceed_with_call(get_lists_entry(Data, Call), Data, Call).

-spec maybe_proceed_with_call(list_cid_entry(), kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_proceed_with_call({NewCallerIdName, NewCallerIdNumber, Dest}, Data, Call) ->
    proceed_with_call(NewCallerIdName, NewCallerIdNumber, Dest, Data, Call);
maybe_proceed_with_call(_, _, Call) ->
    _ = kapps_call_command:answer(Call),
    _ = kapps_call_command:prompt(<<"fault-can_not_be_completed_at_this_time">>, Call),
    kapps_call_command:queued_hangup(Call).

-spec proceed_with_call(ne_binary(), ne_binary(), binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
proceed_with_call(NewCallerIdName, NewCallerIdNumber, Dest, Data, Call) ->
    update_call(Call, NewCallerIdNumber, NewCallerIdName, Dest),
    Number = knm_converters:normalize(Dest),
    maybe_route_to_callflow(Data, Call, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc Update caller id number. If call
%% has a capture group, strip the non capture group digits from
%% request, to and callee_number
%% @end
%%--------------------------------------------------------------------
-spec update_call(kapps_call:call(), ne_binary(), api_binary()) -> 'ok'.
update_call(Call, CIDNumber, CaptureGroup) ->
    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', CIDNumber}
              ,{fun kapps_call:set_caller_id_number/2, CIDNumber}
              ],
    {'ok', C1} = cf_exe:get_call(Call),
    lager:info("setting the caller id number to ~s (from ~s)"
              ,[CIDNumber, kapps_call:caller_id_number(Call)]
              ),
    maybe_strip_features_code(kapps_call:exec(Updates, C1), CaptureGroup).

%%--------------------------------------------------------------------
%% @private
%% @doc Same as update_call/3, but also sets caller id name
%% @end
%%--------------------------------------------------------------------
-spec update_call(kapps_call:call(), ne_binary(), ne_binary(), api_binary()) -> 'ok'.
update_call(Call, CIDNumber, CIDName, CaptureGroup) ->
    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', CIDNumber}
              ,{fun kapps_call:set_caller_id_number/2, CIDNumber}
              ,{fun kapps_call:set_caller_id_name/2, CIDName}
              ],
    {'ok', C1} = cf_exe:get_call(Call),
    lager:info("setting the cid to <~s> ~s (from <~s> ~s)"
              ,[CIDName
               ,CIDNumber
               ,kapps_call:caller_id_name(Call)
               ,kapps_call:caller_id_number(Call)
               ]
              ),
    maybe_strip_features_code(kapps_call:exec(Updates, C1), CaptureGroup).

%%--------------------------------------------------------------------
%% @private
%% @doc If CaptureGroup exists correct request, to and callee_id_number
%% @end
%%--------------------------------------------------------------------
-spec maybe_strip_features_code(kapps_call:call(), api_binary()) -> kapps_call:call().
maybe_strip_features_code(Call, 'undefined') ->
    cf_exe:set_call(Call);
maybe_strip_features_code(Call, CaptureGroup) ->
    Norm = knm_converters:normalize(CaptureGroup),
    Request = list_to_binary([Norm, "@", kapps_call:request_realm(Call)]),
    To = list_to_binary([Norm, "@", kapps_call:to_realm(Call)]),

    lager:info("sending the call onto real destination of: ~s", [Norm]),

    Updates = [{fun kapps_call:set_request/2, Request}
              ,{fun kapps_call:set_to/2, To}
              ,{fun kapps_call:set_callee_id_number/2, Norm}
              ],
    cf_exe:set_call(kapps_call:exec(Updates, Call)).

%%--------------------------------------------------------------------
%% @private
%% @doc Lookup callflow and continue with the call
%% @end
%%--------------------------------------------------------------------
-spec maybe_route_to_callflow(kz_json:object(), kapps_call:call(), ne_binary()) -> 'ok'.
maybe_route_to_callflow(Data, Call, Number) ->
    case cf_flow:lookup(Number, kapps_call:account_id(Call)) of
        {'ok', Flow, 'true'} ->
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

    DefaultMin = DynamicCID#dynamic_cid.default_min_digits,
    DefaultMax = DynamicCID#dynamic_cid.default_max_digits,
    DefaultRegex = DynamicCID#dynamic_cid.default_whitelist,
    DefaultCID = kapps_call:caller_id_number(Call),

    Min = kz_json:get_ne_value(<<"min_digits">>, Data, DefaultMin),
    Max = kz_json:get_ne_value(<<"max_digits">>, Data, DefaultMax),
    Regex = kz_json:get_ne_value(<<"whitelist_regex">>, Data, DefaultRegex),

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
        {'error', 'channel_hungup'} ->
            lager:info("caller hungup while collecting caller id number"),
            cf_exe:stop(Call);
        {'error', _} ->
            _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
            DefaultCID
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get static CID from callflow data
%% @end
%%--------------------------------------------------------------------
-spec get_static_cid_entry(kz_json:object(), kapps_call:call()) -> cid().
get_static_cid_entry(Data, Call) ->
    case kz_json:get_ne_value(<<"caller_id">>, Data) of
        'undefined' ->
            maybe_set_default_cid('undefined', 'undefined', Call);
        NewCallerId ->
            Name = kz_json:get_ne_binary_value(<<"name">>, NewCallerId),
            Number = kz_json:get_ne_binary_value(<<"number">>, NewCallerId),
            maybe_set_default_cid(Name, Number, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Pull in document from database with the callerid switching information inside
%% @end
%%--------------------------------------------------------------------
-spec get_list_entry(kz_json:object(), kapps_call:call()) -> list_cid_entry().
get_list_entry(Data, Call) ->
    ListId = kz_json:get_ne_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),

    case kz_datamgr:open_cache_doc(AccountDb, ListId) of
        {'ok', ListJObj} ->
            {CIDKey, DestNumber} = find_key_and_dest(ListJObj, Data, Call),
            {NewCallerIdName, NewCallerIdNumber} = get_new_caller_id(CIDKey, ListJObj, Call),
            {NewCallerIdName, NewCallerIdNumber, DestNumber};
        {'error', _Reason}=E ->
            lager:info("failed to load match list document ~s: ~p", [ListId, _Reason]),
            E
    end.

-spec find_key_and_dest(kz_json:object(), kz_json:object(), kapps_call:call()) -> {binary(), binary()}.
find_key_and_dest(ListJObj, Data, Call) ->
    case kz_json:get_value(<<"idx_name">>, Data) of
        'undefined' -> find_key_and_dest(ListJObj, Call);
        Idx ->
            Groups = kapps_call:kvs_fetch('cf_capture_groups', Call),
            CIDKey = kz_json:get_value(Idx, Groups),
            Dest = kapps_call:kvs_fetch('cf_capture_group', Call),
            {CIDKey, Dest}
    end.

-spec find_key_and_dest(kz_json:object(), kapps_call:call()) -> {binary(), binary()}.
find_key_and_dest(ListJObj, Call) ->
    LengthDigits = kz_json:get_integer_value(<<"length">>, ListJObj, 2),
    lager:debug("digit length to limit lookup key in number: ~p", [LengthDigits]),
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    <<CIDKey:LengthDigits/binary, Dest/binary>> = CaptureGroup,
    {CIDKey, Dest}.

-spec get_new_caller_id(binary(), kz_json:object(), kapps_call:call()) -> cid().
get_new_caller_id(CIDKey, ListJObj, Call) ->
    JObj = kz_json:get_ne_value(<<"entries">>, ListJObj, kz_json:new()),
    case kz_json:get_value(CIDKey, JObj) of
        'undefined' ->
            maybe_set_default_cid('undefined', 'undefined', Call);
        NewCallerId ->
            Name = kz_json:get_ne_binary_value(<<"name">>, NewCallerId),
            Number = kz_json:get_ne_binary_value(<<"number">>, NewCallerId),
            maybe_set_default_cid(Name, Number, Call)
    end.

-spec get_lists_entry(kz_json:object(), kapps_call:call()) -> list_cid_entry().
get_lists_entry(Data, Call) ->
    ListId = kz_json:get_ne_value(<<"id">>, Data),
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb,<<"lists/entries">>,[{'key', ListId}]) of
        {'ok', Entries} ->
            CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
            <<CIDKey:2/binary, Dest/binary>> = CaptureGroup,
            {NewCallerIdName, NewCallerIdNumber} = cid_key_lookup(CIDKey, Entries, Call),
            {NewCallerIdName, NewCallerIdNumber, Dest};
        {'error', Reason} = E ->
            lager:info("failed to load match list document ~s: ~p", [ListId, Reason]),
            E
    end.

-spec cid_key_lookup(binary(), kz_json:objects(), kapps_call:call()) -> cid().
cid_key_lookup(CIDKey, Entries, Call) ->
    case lists:foldl(fun(Entry, Acc) -> cidkey_wanted(CIDKey, Entry, Acc) end, [], Entries) of
        [{NewCallerIdName, NewCallerIdNumber}|_] ->
            maybe_set_default_cid(NewCallerIdName, NewCallerIdNumber, Call);
        _ ->
            maybe_set_default_cid('undefined', 'undefined', Call)
    end.

-spec cidkey_wanted(binary(), kz_json:object(), proplist()) -> proplist().
cidkey_wanted(CIDKey, Entry, Acc) ->
    case kz_json:get_binary_value([<<"value">>, <<"cid_key">>], Entry) == CIDKey of
        'true' -> Acc ++ [{kz_json:get_ne_binary_value([<<"value">>, <<"cid_name">>], Entry)
                          ,kz_json:get_ne_binary_value([<<"value">>, <<"cid_number">>], Entry)
                          }];
        'false' -> Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Play reject prompt if any of the caller id are empty
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_default_cid(api_ne_binary(), api_ne_binary(), kapps_call:call()) -> cid().
maybe_set_default_cid('undefined', 'undefined', Call) ->
    lager:debug("empty cid entry, set to default value"),
    play_reject_prompt(Call),
    {kapps_call:caller_id_name(Call), kapps_call:caller_id_number(Call)};
maybe_set_default_cid('undefined', Number, Call) ->
    lager:debug("empty cid name, set to default value"),
    {kapps_call:caller_id_name(Call), Number};
maybe_set_default_cid(Name, 'undefined', Call) ->
    lager:debug("empty cid number, set to default value"),
    play_reject_prompt(Call),
    {Name, kapps_call:caller_id_number(Call)};
maybe_set_default_cid(Name, Number, _Call) ->
    {Name, Number}.

%%--------------------------------------------------------------------
%% @private
%% @doc play reject prompts when caller id number is empty or invalid
%% @end
%%--------------------------------------------------------------------
-spec play_reject_prompt(kapps_call:call()) -> 'ok'.
play_reject_prompt(Call) ->
    _ = kapps_call_command:play(
          kz_media_util:get_prompt(
            kapps_config:get_binary(?MOD_CONFIG_CAT, <<"reject_prompt">>, <<"dynamic-cid-invalid_using_default">>)
           ), Call),
    'ok'.
