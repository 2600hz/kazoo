%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_privacy).

-export([maybe_cid_privacy/2
        ,flags/1
        ,has_flags/1, is_anonymous/1
        ,anonymous_caller_id_name/0, anonymous_caller_id_name/1
        ,anonymous_caller_id_number/0, anonymous_caller_id_number/1
        ,should_block_anonymous/1
        ]).

-include("kazoo_endpoint.hrl").

-define(PRIVACY_CAT, <<"privacy">>).
-define(KEY_PRIVACY_MODE, <<"privacy_mode">>).
-define(KEY_LEGACY_ANONYMIZER, [<<"caller_id_options">>, <<"anonymizer">>]).
-define(ANON_NAME, <<"anonymous">>).
-define(ANON_NUMBER, <<"0000000000">>).
-define(KEY_ANON_NAME, <<"privacy_name">>).
-define(KEY_ANON_NUMBER, <<"privacy_number">>).

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-define(HIDE_BOTH, <<"kazoo">>).
-define(NO_HIDE_MODE, <<"sip">>).
-define(HIDE_NAME, <<"hide_name">>).
-define(HIDE_NUMBER, <<"hide_number">>).

-define(CALLER_SCREEN_BIT, <<"Caller-Screen-Bit">>).
-define(CALLER_PRIVACY_NAME, <<"Caller-Privacy-Hide-Name">>).
-define(CALLER_PRIVACY_NUMBER, <<"Caller-Privacy-Hide-Number">>).

-type cid_name() :: api_binary().
-type cid_number() :: api_binary().
-type cid() :: {cid_name(), cid_number()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Would anonymize caller id if screen and hide parameters in
%% CCVs are presents and system is configured to hide some part of
%% caller id or both or bot hiding at all.
%% @end
%%--------------------------------------------------------------------
-spec maybe_cid_privacy(kz_proplist() | kz_json:object(), cid()) -> cid().
maybe_cid_privacy(Props, Default) when is_list(Props) ->
    maybe_cid_privacy(kz_json:from_list(Props), Default);
maybe_cid_privacy(JObj, Default) ->
    PrivacyMode = kz_json:get_ne_binary_value(<<"Privacy-Mode">>, JObj),
    case caller_privacy_mode(JObj) of
        'false' -> Default;
        ?NO_HIDE_MODE -> Default;
        HideMode ->
            lager:debug("caller privacy flags are set, maybe overriding caller id"),
            maybe_anonymize_cid(PrivacyMode, HideMode, JObj, Default)
    end.

-spec flags(kz_proplist() | kz_json:object()) -> kz_proplist().
flags(Props) when is_list(Props) ->
    flags(kz_json:from_list(Props));
flags(JObj) ->
    [{?CALLER_SCREEN_BIT, caller_screen_bit(JObj)}
    ,{?CALLER_PRIVACY_NAME, caller_privacy_name(JObj)}
    ,{?CALLER_PRIVACY_NUMBER, caller_privacy_number(JObj)}
    ].

-spec has_flags(kz_proplist() | kz_json:object()) -> boolean().
has_flags(Props) when is_list(Props) ->
    has_flags(kz_json:from_list(Props));
has_flags(JObj) ->
    caller_screen_bit(JObj)
        andalso (caller_privacy_name(JObj)
                 orelse caller_privacy_number(JObj)
                ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find out if we call should be blocked if it's anonymous and Account
%% or system is configured to block anonymous calls
%% @end
%%--------------------------------------------------------------------
-spec should_block_anonymous(kz_json:object()) -> boolean().
should_block_anonymous(JObj) ->
    AccountId = get_value(<<"Account-ID">>, JObj),
    ShouldBlock = kapps_account_config:get_global(AccountId
                                                 ,?PRIVACY_CAT
                                                 ,<<"block_anonymous_caller_id">>
                                                 ,'false'
                                                 ),
    case {kz_term:is_true(ShouldBlock), is_anonymous(JObj)} of
        {'true', 'true'} ->
            lager:info("block anonymous call, account_id: ~s", [AccountId]),
            'true';
        {'true', 'false'} ->
            lager:info("passing non-anonymous call, account_id: ~s", [AccountId]),
            'false';
        _Check ->
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Checks all possible variables to see if the incoming call is anonymous
%% @end
%%--------------------------------------------------------------------
-spec is_anonymous(kz_json:object()) -> boolean().
is_anonymous(JObj) ->
    IsPrivacyNumber = kz_term:is_true(get_value(<<"Caller-Privacy-Number">>, JObj, 'false')),
    IsPrivacyName = kz_term:is_true(get_value(<<"Caller-Privacy-Name">>, JObj, 'false')),
    IsCallerNumberZero = is_zero(kz_json:get_value(<<"Caller-ID-Number">>, JObj)),
    HasPrivacyFlags = has_flags(JObj),
    IsCallerNumberZero
        orelse IsPrivacyName
        orelse IsPrivacyNumber
        orelse HasPrivacyFlags.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Default anonymous Caller IDs from System wide config, or Account
%% @end
%%--------------------------------------------------------------------
-spec anonymous_caller_id_name() -> ne_binary().
anonymous_caller_id_name() ->
    kapps_config:get_binary(?PRIVACY_CAT, ?KEY_ANON_NAME, ?ANON_NAME).

-spec anonymous_caller_id_name(api_binary()) -> ne_binary().
anonymous_caller_id_name('undefined') ->
    anonymous_caller_id_name();
anonymous_caller_id_name(AccountId) ->
    kapps_account_config:get_global(AccountId, ?PRIVACY_CAT, ?KEY_ANON_NAME, ?ANON_NAME).

-spec anonymous_caller_id_number() -> ne_binary().
anonymous_caller_id_number() ->
    kapps_config:get_binary(?PRIVACY_CAT, ?KEY_ANON_NUMBER, ?ANON_NUMBER).

-spec anonymous_caller_id_number(api_binary()) -> ne_binary().
anonymous_caller_id_number('undefined') ->
    anonymous_caller_id_number();
anonymous_caller_id_number(AccountId) ->
    kapps_account_config:get_global(AccountId, ?PRIVACY_CAT, ?KEY_ANON_NUMBER, ?ANON_NUMBER).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks for screen bits and caller id hide parameters
%% @end
%%--------------------------------------------------------------------
-spec caller_privacy_mode(kz_json:object()) -> ne_binary() | 'false'.
caller_privacy_mode(JObj) ->
    caller_screen_bit(JObj)
        andalso caller_privacy_mode(caller_privacy_name(JObj), caller_privacy_number(JObj)).

-spec caller_privacy_mode(boolean(), boolean()) -> ne_binary().
caller_privacy_mode('true', 'true') -> ?HIDE_BOTH;
caller_privacy_mode('true', 'false') -> ?HIDE_NAME;
caller_privacy_mode('false', 'true') -> ?HIDE_NUMBER;
caller_privacy_mode('false', 'false') -> ?NO_HIDE_MODE.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make some/all/no part of caller id anonymous base on system config
%% @end
%%--------------------------------------------------------------------
-spec maybe_anonymize_cid(api_binary(), ne_binary(), kz_json:object(), cid()) -> cid().
maybe_anonymize_cid('undefined', HideMode, JObj, Default) ->
    maybe_anonymize_cid(get_default_privacy_mode(JObj), HideMode, JObj, Default);
maybe_anonymize_cid(?NO_HIDE_MODE, _, _, Default) ->
    lager:debug("not overriding caller id"),
    Default;
maybe_anonymize_cid(?HIDE_BOTH, HideMode, JObj, Default) ->
    lager:info("overriding caller id to maintain privacy"),
    AccountId = get_value(<<"Account-ID">>, JObj),
    anonymize_cid(AccountId, Default, HideMode);
maybe_anonymize_cid(PrivacyMode, HideMode, JObj, Default) ->
    Mode = hide_mode(PrivacyMode, HideMode),
    AccountId = get_value(<<"Account-ID">>, JObj),
    anonymize_cid(AccountId, Default, Mode).

-spec hide_mode(ne_binary(), ne_binary()) -> ne_binary().
hide_mode(?HIDE_NAME, ?HIDE_NAME) ->
    lager:info("overriding caller id name to maintain privacy"),
    ?HIDE_NAME;
hide_mode(?HIDE_NAME, _HideMode) ->
    lager:debug("ignoring ccv's caller privacy hide number, just overriding caller id name", [_HideMode]),
    ?HIDE_NAME;
hide_mode(?HIDE_NUMBER, ?HIDE_NUMBER) ->
    lager:info("overriding caller id number to maintain privacy"),
    ?HIDE_NUMBER;
hide_mode(?HIDE_NUMBER, _HideMode) ->
    lager:debug("ignoring ccv's caller privacy hide name, just overriding caller id number", [_HideMode]),
    ?HIDE_NUMBER.

-spec anonymize_cid(ne_binary(), cid(), ne_binary()) -> cid().
anonymize_cid(_AccountId, Default, ?NO_HIDE_MODE) ->
    Default;
anonymize_cid(AccountId, _Default, ?HIDE_BOTH) ->
    {anonymous_caller_id_name(AccountId)
    ,anonymous_caller_id_number(AccountId)
    };
anonymize_cid(AccountId, {_, Number}, ?HIDE_NAME) ->
    {anonymous_caller_id_name(AccountId), Number};
anonymize_cid(AccountId, {Name, _}, ?HIDE_NUMBER) ->
    {Name, anonymous_caller_id_number(AccountId)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get default privacy settings from system_config or from
%% legacy anonymizer setting on account's document
%% @end
%%--------------------------------------------------------------------
-spec get_default_privacy_mode(kz_json:object()) -> ne_binary().
get_default_privacy_mode(JObj) ->
    AccountId = get_value(<<"Account-ID">>, JObj, get_master_account()),
    case get_legacy_anonymizer(kz_account:fetch(AccountId)) of
        'undefined' ->
            kapps_account_config:get_global(AccountId
                                           ,?PRIVACY_CAT
                                           ,?KEY_PRIVACY_MODE
                                           ,?HIDE_BOTH
                                           );
        ?HIDE_BOTH -> ?HIDE_BOTH;
        _Other -> ?NO_HIDE_MODE
    end.

-spec get_legacy_anonymizer({'ok', kz_json:object()} | {'error', any()}) ->
                                   api_ne_binary().
get_legacy_anonymizer({'error', _}) -> 'undefined';
get_legacy_anonymizer({'ok', JObj}) ->
    kz_json:get_ne_binary_value(?KEY_LEGACY_ANONYMIZER, JObj).

-spec get_master_account() -> api_binary().
get_master_account() ->
    case kapps_util:get_master_account_id() of
        {'ok', AccountId} -> AccountId;
        {'error', _} -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Look for Caller Screens and CID Privacy Hide value, default to 'false'
%% @end
%%--------------------------------------------------------------------
-spec caller_screen_bit(kz_json:object()) -> boolean().
caller_screen_bit(JObj) -> kz_term:is_true(get_value(?CALLER_SCREEN_BIT, JObj, 'false')).

-spec caller_privacy_name(kz_json:object()) -> boolean().
caller_privacy_name(JObj) -> kz_term:is_true(get_value(?CALLER_PRIVACY_NAME, JObj, 'false')).

-spec caller_privacy_number(kz_json:object()) -> boolean().
caller_privacy_number(JObj) -> kz_term:is_true(get_value(?CALLER_PRIVACY_NUMBER, JObj, 'false')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Look into Caller ID Number to see if it starts with at least two 00
%% (a common way to make caller id number anonymous)
%% @end
%%--------------------------------------------------------------------
-spec is_zero(api_ne_binary()) -> boolean().
is_zero(?NE_BINARY=Number) ->
    case re:run(erlang:binary_to_list(Number), "^\\+?00+\$", ['global']) of
        {'match', _} -> 'true';
        _ -> 'false'
    end;
is_zero(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get Key from JObj, if not found look into CCV
%% @end
%%--------------------------------------------------------------------
-spec get_value(ne_binary(), kz_json:object()) -> any().
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').

-spec get_value(ne_binary(), kz_json:object(), any()) -> any().
get_value(Key, JObj, Default) ->
    case kz_json:get_first_defined([Key, ?CCV(Key)], JObj) of
        'undefined' -> Default;
        Value ->
            case kz_term:is_empty(Value) of
                'true' -> Default;
                'false' -> Value
            end
    end.
