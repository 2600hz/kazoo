%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_privacy).

-export([maybe_cid_privacy/3
        ,privacy_flags/1
        ,has_privacy/1
        ]).

-include("kazoo_endpoint.hrl").

-define(PRIVACY_CAT, <<"privacy">>).
-define(KEY_PRIVACY_MODE, <<"privacy_mode">>).
-define(KEY_LEGACY_ANONYMIZER, [<<"caller_id_options">>, <<"anonymizer">>]).

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
-spec maybe_cid_privacy(api_binary(), kz_proplist() | kz_json:object(), cid()) -> cid().
maybe_cid_privacy(PrivacyMode, CCVs, Default) when is_list(CCVs) ->
    maybe_cid_privacy(PrivacyMode, kz_json:from_list(CCVs), Default);
maybe_cid_privacy(PrivacyMode, CCVs, Default) ->
    %%io:format("PrivacyMode ~p~n CCVs ~p~n~n", [PrivacyMode, CCVs]),
    case caller_privacy_mode(CCVs) of
        ?NO_HIDE_MODE -> Default;
        HideMode ->
            lager:debug("caller privacy flags are set, maybe overriding caller id"),
            maybe_anonymize_cid(PrivacyMode, HideMode, CCVs, Default)
    end.

-spec privacy_flags(kz_proplist() | kz_json:object()) -> kz_proplist().
privacy_flags(Props) when is_list(Props) ->
    privacy_flags(kz_json:from_list(Props));
privacy_flags(JObj) ->
    [{?CALLER_SCREEN_BIT, caller_screen_bit(JObj)}
    ,{?CALLER_PRIVACY_NAME, caller_privacy_name(JObj)}
    ,{?CALLER_PRIVACY_NUMBER, caller_privacy_number(JObj)}
    ].

-spec has_privacy(kz_proplist() | kz_json:object()) -> boolean().
has_privacy(Props) when is_list(Props) ->
    has_privacy(kz_json:from_list(Props));
has_privacy(JObj) ->
    caller_screen_bit(JObj)
        andalso (caller_privacy_name(JObj)
                     orelse caller_privacy_number(JObj)
                ).

-spec caller_screen_bit(kz_json:object()) -> boolean().
caller_screen_bit(JObj) -> kz_json:is_true(?CALLER_SCREEN_BIT, JObj, 'false').

-spec caller_privacy_name(kz_json:object()) -> boolean().
caller_privacy_name(JObj) -> kz_json:is_true(?CALLER_PRIVACY_NAME, JObj, 'false').

-spec caller_privacy_number(kz_json:object()) -> boolean().
caller_privacy_number(JObj) -> kz_json:is_true(?CALLER_PRIVACY_NUMBER, JObj, 'false').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check CCVs for screen bits and caller id hide parameters
%% @end
%%--------------------------------------------------------------------
-spec caller_privacy_mode(kz_json:object()) -> ne_binary().
caller_privacy_mode(CCVs) ->
    caller_screen_bit(CCVs)
        andalso caller_privacy_mode(caller_privacy_name(CCVs), caller_privacy_number(CCVs)).

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
maybe_anonymize_cid('undefined', HideMode, CCVs, Default) ->
    maybe_anonymize_cid(get_default_privacy_mode(CCVs), HideMode, CCVs, Default);
maybe_anonymize_cid(?NO_HIDE_MODE, _, _, Default) ->
    lager:debug("not overriding caller id"),
    Default;
maybe_anonymize_cid(?HIDE_BOTH, HideMode, _, Default) ->
    lager:info("overriding caller id to maintain privacy"),
    anonymize_cid(Default, HideMode);
maybe_anonymize_cid(PrivacyMode, HideMode, _CCVs, Default) ->
    Mode = hide_mode(PrivacyMode, HideMode),
    anonymize_cid(Default, Mode).

-spec hide_mode(ne_binary(), ne_binary()) -> ne_binary().
hide_mode(?HIDE_NAME, ?HIDE_NAME) ->
    lager:info("overriding caller id name to maintain privacy"),
    ?HIDE_NAME;
hide_mode(?HIDE_NAME, _HideMode) ->
    lager:debug("not allowing ~s privacy mode", [_HideMode]),
    ?NO_HIDE_MODE;
hide_mode(?HIDE_NUMBER, ?HIDE_NUMBER) ->
    lager:info("overriding caller id number to maintain privacy"),
    ?HIDE_NUMBER;
hide_mode(?HIDE_NUMBER, _HideMode) ->
    lager:debug("not allowing ~s privacy mode", [_HideMode]),
    ?NO_HIDE_MODE.

-spec anonymize_cid(cid(), ne_binary()) -> cid().
anonymize_cid(Default, ?NO_HIDE_MODE) ->
    Default;
anonymize_cid(_Default, ?HIDE_BOTH) ->
    {kz_util:anonymous_caller_id_name()
    ,kz_util:anonymous_caller_id_number()
    };
anonymize_cid({_, Number}, ?HIDE_NAME) ->
    {kz_util:anonymous_caller_id_name(), Number};
anonymize_cid({Name, _}, ?HIDE_NUMBER) ->
    {Name, kz_util:anonymous_caller_id_number()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get default privacy settings from system_config or from
%% legacy anonymizer setting on account's document
%% @end
%%--------------------------------------------------------------------
-spec get_default_privacy_mode(kz_json:object()) -> ne_binary().
get_default_privacy_mode(CCVs) ->
    AccountId = kz_json:get_ne_value(<<"Account-ID">>
                                    ,CCVs
                                    ,get_master_account()
                                    ),
    case get_legacy_anonymizer(kz_account:fetch(AccountId)) of
        'undefined' ->
            kapps_account_config:get_global(AccountId
                                           ,?PRIVACY_CAT
                                           ,?KEY_PRIVACY_MODE,
                                           ?HIDE_BOTH
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
