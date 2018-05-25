%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Allow the user to change their Caller ID based on the action.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>How to collect Caller ID: `lists' (`list' has same effect), `static' or `manual'. Default is `manual'.</dd>
%%%
%%%   <dt>`media_id'</dt>
%%%   <dd>ID of the media prompt to play before starting collecting DTMF.</dd>
%%%
%%%   <dt>`id'</dt>
%%%   <dd>ID if the list document to use if action is `list' or `lists'. Required if the action is `list' or `lists'.</dd>
%%%
%%%   <dt>`interdigit'</dt>
%%%   <dd><strong>Optional: </strong>How long to wait for the next DTMF, in milliseconds</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%% @author William Lloyd
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_dynamic_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-define(REJECT_PROMPT
       ,kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"reject_prompt">>, <<"dynamic-cid-invalid_using_default">>)
       ).

-record(prompts
       ,{accept_tone =
             kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>)
         :: kz_term:ne_binary()
        ,reject_tone = kz_media_util:get_prompt(?REJECT_PROMPT) :: kz_term:api_ne_binary()
        ,default_prompt =
             kz_media_util:get_prompt(
               kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"default_prompt">>, <<"dynamic-cid-enter_cid">>)
              )
         :: kz_term:ne_binary()
        }).
-type prompts() :: #prompts{}.

-record(dynamic_cid
       ,{prompts = #prompts{} :: prompts()
        ,default_max_digits = kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_digits">>, 10) :: integer()
        ,default_min_digits = kapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_digits">>, 10) :: integer()
        ,default_whitelist = kapps_config:get_binary(?MOD_CONFIG_CAT, <<"whitelist_regex">>, <<"\\d+">>) :: kz_term:ne_binary()
        }
       ).

-type cid() :: {kz_term:ne_binary(), kz_term:ne_binary()}.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    Action = kz_json:get_ne_binary_value(<<"action">>, Data),
    handle(Data, Call, Action, CaptureGroup).

-spec handle(kz_json:object(), kapps_call:call(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> 'ok'.
handle(Data, Call, <<"list">>, ?NE_BINARY = _CaptureGroup) ->
    lager:info("using account's lists/entries view to get new cid info"),
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

%%------------------------------------------------------------------------------
%% @doc Handle manual mode of dynamic cid
%% @end
%%------------------------------------------------------------------------------
-spec handle_manual(kz_json:object(), kapps_call:call(), kz_term:api_ne_binary()) -> 'ok'.
handle_manual(Data, Call, CaptureGroup) ->
    case collect_cid_number(Data, Call) of
        {'ok', CID} ->
            Number = cf_util:normalize_capture_group(CaptureGroup),
            kapps_call_command:flush_dtmf(Call),
            maybe_update_call_and_continue(Call, CID, Number);
        {'error', 'channel_hungup'} ->
            lager:info("caller hungup while collecting caller id number"),
            cf_exe:stop(Call)
    end.


%%------------------------------------------------------------------------------
%% @doc Handle static mode of dynamic cid
%% @end
%%------------------------------------------------------------------------------
-spec handle_static(kz_json:object(), kapps_call:call(), kz_term:api_ne_binary()) -> 'ok'.
handle_static(Data, Call, CaptureGroup) ->
    {CIDName, CIDNumber} = get_static_cid_entry(Data, Call),
    Number = cf_util:normalize_capture_group(CaptureGroup),
    maybe_update_call_and_continue('undefined', Call, CIDNumber, CIDName, Number).

%%------------------------------------------------------------------------------
%% @doc Read CID info from a list of CID defined in database
%% @end
%%------------------------------------------------------------------------------
-type list_cid_entry() :: {kz_term:ne_binary(), kz_term:ne_binary(), binary()} |
                          {'error', kz_datamgr:data_error()}.

-spec handle_list(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_list(Data, Call) ->
    maybe_proceed_with_call(get_list_entry(Data, Call), Data, Call).

-spec handle_lists(kz_json:object(), kapps_call:call()) -> 'ok'.
handle_lists(Data, Call) ->
    ListId = kz_json:get_ne_binary_value(<<"id">>, Data),
    maybe_proceed_with_call(get_caller_id_from_entries(Call, ListId, 'undefined'), Data, Call).

-spec maybe_proceed_with_call(list_cid_entry(), kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_proceed_with_call({CIDName, CIDNumber, Destination}, Data, Call) ->
    Number = cf_util:normalize_capture_group(Destination),
    maybe_update_call_and_continue(Data, Call, CIDNumber, CIDName, Number);
maybe_proceed_with_call(_, _, Call) ->
    lager:debug("failed to find cid name/number and destination from list(s), hanging up."),
    cf_exe:stop_bad_destination(Call).

%%------------------------------------------------------------------------------
%% @doc Update caller id number. If call
%% has a capture group, strip the non capture group digits from
%% request, to and callee_number
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_call_and_continue(kapps_call:call(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> 'ok'.
maybe_update_call_and_continue(Call, _, 'undefined') ->
    lager:debug("remaining capture group is empty and can not be set as destination, hanging up."),
    cf_exe:stop_bad_destination(Call);
maybe_update_call_and_continue(Call, CIDNumber, Destination) ->
    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', CIDNumber}
              ,{fun kapps_call:set_caller_id_number/2, CIDNumber}
              ],
    {'ok', C1} = cf_exe:get_call(Call),
    lager:info("setting the caller id number to ~s (from ~s)"
              ,[CIDNumber, kapps_call:caller_id_number(Call)]
              ),
    maybe_strip_features_code(kapps_call:exec(Updates, C1), Destination),
    cf_exe:continue(Call).

%%------------------------------------------------------------------------------
%% @doc Same as maybe_update_call_and_continue/3, but also sets caller id name
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_call_and_continue(kz_term:api_object(), kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> 'ok'.
maybe_update_call_and_continue(_, Call, _, _, 'undefined') ->
    lager:debug("remaining capture group is empty and can not be set as destination, hanging up."),
    cf_exe:stop_bad_destination(Call);
maybe_update_call_and_continue('undefined', Call, CIDNumber, CIDName, Destination) ->
    %% handling static action
    update_call(Call, CIDNumber, CIDName, Destination),
    cf_exe:continue(Call);
maybe_update_call_and_continue(Data, Call, CIDNumber, CIDName, Destination) ->
    %% handling list, lists actions
    update_call(Call, CIDNumber, CIDName, Destination),
    maybe_route_to_callflow(Data, Call, Destination).

-spec update_call(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_call(Call, CIDNumber, CIDName, Destination) ->
    Updates = [{fun kapps_call:kvs_store/3, 'dynamic_cid', {CIDNumber, CIDName}}
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
    maybe_strip_features_code(kapps_call:exec(Updates, C1), Destination).

%%------------------------------------------------------------------------------
%% @doc If Destination exists correct "request", "to" and "callee_id_number"
%% @end
%%------------------------------------------------------------------------------
-spec maybe_strip_features_code(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
maybe_strip_features_code(Call, Number) ->
    Request = list_to_binary([Number, "@", kapps_call:request_realm(Call)]),
    To = list_to_binary([Number, "@", kapps_call:to_realm(Call)]),

    lager:info("sending the call onto real destination of: ~s", [Number]),

    Updates = [{fun kapps_call:set_request/2, Request}
              ,{fun kapps_call:set_to/2, To}
              ,{fun kapps_call:set_callee_id_number/2, Number}
              ],
    cf_exe:set_call(kapps_call:exec(Updates, Call)).

%%------------------------------------------------------------------------------
%% @doc Lookup callflow and continue with the call if we have a destination number
%% @end
%%------------------------------------------------------------------------------
-spec maybe_route_to_callflow(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
maybe_route_to_callflow(Data, Call, Number) ->
    case cf_flow:lookup(Number, kapps_call:account_id(Call)) of
        {'ok', Flow, 'true'} ->
            maybe_restrict_call(Data, Call, Number, Flow);
        _ ->
            lager:info("failed to find a callflow to satisfy ~s", [Number]),
            _ = kapps_call_command:b_prompt(<<"disa-invalid_extension">>, Call),
            cf_exe:stop(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_restrict_call(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kzd_callflow:doc()) -> 'ok'.
maybe_restrict_call(Data, Call, Number, Flow) ->
    case should_restrict_call(Data, Call, Number) of
        'true' ->
            lager:info("Not allowed to call this destination, terminate", []),
            _ = kapps_call_command:answer(Call),
            _ = kapps_call_command:prompt(<<"cf-unauthorized_call">>, Call),
            _ = kapps_call_command:queued_hangup(Call),
            'ok';
        'false' ->
            cf_exe:branch(kzd_callflow:flow(Flow), Call)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_restrict_call(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) ->
                                  boolean().
should_restrict_call(Data, Call, Number) ->
    case kz_json:is_true(<<"enforce_call_restriction">>, Data, 'true') of
        'true' -> should_restrict_call(Call, Number);
        'false' ->
            lager:info("not enforcing call restrictions"),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_restrict_call(kapps_call:call(), kz_term:ne_binary()) -> boolean().
should_restrict_call(Call, Number) ->
    case  kz_endpoint:get(Call) of
        {'error', _} -> 'false';
        {'ok', JObj} ->
            Classification = knm_converters:classify(Number),
            lager:info("classified number as ~s", [Classification]),
            kz_json:get_ne_binary_value([<<"call_restriction">>, Classification, <<"action">>], JObj) =:= <<"deny">>
    end.

%%------------------------------------------------------------------------------
%% @doc Collect CID number from user
%% @end
%%------------------------------------------------------------------------------
-spec collect_cid_number(kz_json:object(), kapps_call:call()) ->
                                {'ok', kz_term:ne_binary()} |
                                {'error', 'channel_hungup'}.
collect_cid_number(Data, Call) ->
    DynamicCID = #dynamic_cid{},
    Prompts = DynamicCID#dynamic_cid.prompts,
    _ = kapps_call_command:b_play(<<"silence_stream://100">>, Call),

    Media = case kz_json:get_ne_binary_value(<<"media_id">>, Data) of
                'undefined' -> Prompts#prompts.default_prompt;
                Else -> Else
            end,

    DefaultMin = DynamicCID#dynamic_cid.default_min_digits,
    DefaultMax = DynamicCID#dynamic_cid.default_max_digits,
    DefaultRegex = DynamicCID#dynamic_cid.default_whitelist,
    DefaultCID = kapps_call:caller_id_number(Call),

    Min = kz_json:get_integer_value(<<"min_digits">>, Data, DefaultMin),
    Max = kz_json:get_integer_value(<<"max_digits">>, Data, DefaultMax),
    Regex = kz_json:get_ne_binary_value(<<"whitelist_regex">>, Data, DefaultRegex),

    Interdigit = kz_json:get_integer_value(<<"interdigit_timeout">>
                                          ,Data
                                          ,kapps_call_command:default_interdigit_timeout()
                                          ),

    NoopId = kapps_call_command:play(Media, Call),

    CollectTimeout = kapps_call_command:default_collect_timeout(),
    case kapps_call_command:collect_digits(Max, CollectTimeout, Interdigit, NoopId, Call) of
        {'ok', <<>>} ->
            _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
            {'ok', DefaultCID};
        {'ok', Digits} ->
            case re:run(Digits, Regex) of
                {'match', _} when byte_size(Digits) >= Min ->
                    kapps_call_command:play(Prompts#prompts.accept_tone, Call),
                    {'ok', Digits};
                _ ->
                    _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
                    {'ok', DefaultCID}
            end;
        {'error', 'channel_hungup'} ->
            {'error', 'channel_hungup'};
        {'error', _} ->
            _ = kapps_call_command:play(Prompts#prompts.reject_tone, Call),
            {'ok', DefaultCID}
    end.

%%------------------------------------------------------------------------------
%% @doc Get static CID from callflow data
%% @end
%%------------------------------------------------------------------------------
-spec get_static_cid_entry(kz_json:object(), kapps_call:call()) -> cid().
get_static_cid_entry(Data, Call) ->
    case kz_json:get_json_value(<<"caller_id">>, Data) of
        'undefined' ->
            maybe_set_default_cid('undefined', 'undefined', Call);
        NewCallerId ->
            Name = kz_json:get_ne_binary_value(<<"name">>, NewCallerId),
            Number = kz_json:get_ne_binary_value(<<"number">>, NewCallerId),
            maybe_set_default_cid(Name, Number, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Pull in document from database with the caller id switching information inside
%% @end
%%------------------------------------------------------------------------------

-type key_dest() :: 'undefined' | {kz_term:ne_binary(), binary()}.

-spec get_list_entry(kz_json:object(), kapps_call:call()) -> list_cid_entry().
get_list_entry(Data, Call) ->
    ListId = kz_json:get_ne_binary_value(<<"id">>, Data),
    get_caller_id_from_entries(Call, ListId, maybe_key_and_dest_using_data(Data, Call)).

-spec maybe_key_and_dest_using_data(kz_json:object(), kapps_call:call()) -> key_dest().
maybe_key_and_dest_using_data(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"idx_name">>, Data) of
        'undefined' -> 'undefined';
        Idx ->
            Groups = kapps_call:kvs_fetch('cf_capture_groups', Call),
            CIDKey = kz_json:get_ne_binary_value(Idx, Groups),
            Destination = kapps_call:kvs_fetch('cf_capture_group', Call),
            {CIDKey, Destination}
    end.

-spec get_caller_id_from_entries(kapps_call:call(), kz_term:api_ne_binary(), key_dest()) -> list_cid_entry().
get_caller_id_from_entries(_, 'undefined', _) ->
    lager:warning("list id is missing"),
    {'error', 'not_found'};
get_caller_id_from_entries(Call, ListId, KeyDest) ->
    case kz_datamgr:get_results(kapps_call:account_db(Call), <<"lists/entries">>, [{'key', ListId}]) of
        {'ok', Entries} ->
            lager:debug("trying to find new caller id from ~b list entries", [length(Entries)]),
            get_new_caller_id(Call, Entries, ListId, KeyDest);
        {'error', _Reason}=Error ->
            lager:info("failed to load entry documents ~s: ~p", [ListId, _Reason]),
            Error
    end.

-spec get_new_caller_id(kapps_call:call(), kz_json:objects(), kz_term:ne_binary(), key_dest()) -> list_cid_entry().
get_new_caller_id(Call, [], _ListId, {_, Destination}) ->
    lager:warning("no entries were found in list ~p", [_ListId]),
    {CidName, CidNumber} = maybe_set_default_cid('undefined', 'undefined', Call),
    {CidName, CidNumber, Destination};
get_new_caller_id(Call, [], ListId, 'undefined') ->
    lager:warning("no entries were found, maybe finding destination number using specified index"),
    LengthDigits = get_cid_length_from_list_document(Call, ListId),
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    try <<_:LengthDigits/binary, Destination/binary>> = CaptureGroup,
         {CidName, CidNumber} = maybe_set_default_cid('undefined', 'undefined', Call),
         {CidName, CidNumber, Destination}
    catch _E:_T ->
            lager:warning("failed to get cid_key (with length ~b) and destination number: ~p:~p", [LengthDigits, _E, _T]),
            {'error', 'not_found'}
    end;
get_new_caller_id(Call, [JObj | Entries], ListId, KeyDest) ->
    Entry = kz_json:get_value(<<"value">>, JObj),

    case get_key_and_dest(Call, Entry, KeyDest) of
        {'error', _}=Error ->
            Error;
        {CIDKey, Destination} ->
            case kz_json:get_ne_binary_value(<<"capture_group_key">>, Entry) of
                CIDKey ->
                    Name = kz_json:get_ne_binary_value(<<"name">>, Entry),
                    Number = kz_json:get_ne_binary_value(<<"number">>, Entry),
                    {CidName, CidNumber} = maybe_set_default_cid(Name, Number, Call),
                    {CidName, CidNumber, Destination};
                _ ->
                    get_new_caller_id(Call, Entries, ListId, KeyDest)
            end
    end.

-spec get_key_and_dest(kapps_call:call(), kz_json:objects(), key_dest()) -> key_dest() | {'error', any()}.
get_key_and_dest(_, _, {CIDKey, _}=KeyDest) ->
    case not kz_term:is_ne_binary(CIDKey) of
        'true' -> KeyDest;
        'false' ->
            {'error', <<"key_dest_failed">>}
    end;
get_key_and_dest(Call, Entry, 'undefined') ->
    LengthDigits = kz_json:get_integer_value(<<"capture_group_length">>, Entry, 2),
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),

    try <<CIDKey:LengthDigits/binary, Destination/binary>> = CaptureGroup,
         {CIDKey, Destination}
    catch _E:_T ->
            lager:warning("failed to get cid_key (with length ~b) and destination number: ~p:~p", [LengthDigits, _E, _T]),
            {'error', <<"entry_failed">>}
    end.

-spec get_cid_length_from_list_document(kapps_call:call(), kz_term:ne_binary()) -> non_neg_integer().
get_cid_length_from_list_document(Call, ListId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), ListId) of
        {'ok', ListJObj} ->
            case kz_json:get_integer_value(<<"length">>, ListJObj, 2) of
                I when is_integer(I), I > 0 -> I;
                _ ->
                    lager:info("cid length from ~s is least than '1', using default '2'", [ListId]),
                    2
            end;
        {'error', _Reason} ->
            lager:info("failed to load list document ~s using default length '2': ~p", [ListId, _Reason]),
            2
    end.

%%------------------------------------------------------------------------------
%% @doc Play reject prompt if any of the caller id are empty
%% @end
%%------------------------------------------------------------------------------
-spec maybe_set_default_cid(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kapps_call:call()) -> cid().
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

%%------------------------------------------------------------------------------
%% @doc play reject prompts when caller id number is empty or invalid
%% @end
%%------------------------------------------------------------------------------
-spec play_reject_prompt(kapps_call:call()) -> 'ok'.
play_reject_prompt(Call) ->
    _ = kapps_call_command:play(kapps_call:get_prompt(Call, ?REJECT_PROMPT), Call),
    'ok'.
