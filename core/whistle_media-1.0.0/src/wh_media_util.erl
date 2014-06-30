%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_media_util).


-export([recording_url/2]).
-export([base_url/1, base_url/2, base_url/3]).
-export([convert_stream_type/1]).
-export([media_path/1, media_path/2]).
-export([max_recording_time_limit/0]).
-export([get_prompt/1, get_prompt/2, get_prompt/3
         ,default_prompt_language/0, default_prompt_language/1
         ,prompt_language/1, prompt_language/2
         ,prompt_id/1, prompt_id/2
         ,prompt_path/1, prompt_path/2
        ]).

-include("whistle_media.hrl").

-define(USE_HTTPS, whapps_config:get_is_true(?CONFIG_CAT, <<"use_https">>, 'false')).
-define(AUTH_PLAYBACK, whapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_playback">>, 'false')).
-define(AUTH_USERNAME, whapps_config:get_string(?CONFIG_CAT, <<"proxy_username">>, wh_util:rand_hex_binary(8))).
-define(AUTH_PASSWORD, whapps_config:get_string(?CONFIG_CAT, <<"proxy_password">>, wh_util:rand_hex_binary(8))).
-define(USE_AUTH_STORE, whapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_store">>, 'true')).

-spec recording_url(ne_binary(), wh_json:object()) -> ne_binary().
recording_url(CallId, Data) ->
    Format = wh_json:get_value(<<"format">>, Data, <<".mp3">>),
    Url = wh_json:get_value(<<"url">>, Data, <<>>),
    <<Url/binary, "call_recording_", CallId/binary, ".", Format/binary>>.

-spec max_recording_time_limit() -> ?SECONDS_IN_HOUR.
max_recording_time_limit() ->
    whapps_config:get_integer(?WHS_CONFIG_CAT, <<"max_recording_time_limit">>, ?SECONDS_IN_HOUR).

base_url(Host) ->
    Port = wh_couch_connections:get_port(),
    base_url(Host, Port).

base_url(Host, Port) ->
    base_url(Host, Port, 'proxy_playback').

base_url(Host, Port, 'direct_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, [], []);
        'true' ->
            {Username, Password} = wh_couch_connections:get_creds(),
            build_url(Host, Port, Username, Password)
    end;
base_url(Host, Port, 'proxy_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, [], []);
        'true' -> build_url(Host, Port, ?AUTH_USERNAME, ?AUTH_PASSWORD)
    end;
base_url(Host, Port, 'direct_store') ->
    case ?USE_AUTH_STORE of
        'false' -> build_url(Host, Port, [], []);
        'true' ->
            {Username, Password} = wh_couch_connections:get_creds(),
            build_url(Host, Port, Username, Password)
    end;
base_url(Host, Port, 'proxy_store') ->
    case ?USE_AUTH_STORE of
        'false' -> build_url(Host, Port, [], []);
        'true' -> build_url(Host, Port, ?AUTH_USERNAME, ?AUTH_PASSWORD)
    end.

build_url(H, P, [], []) ->
    Scheme = case ?USE_HTTPS of
                 'true' -> <<"https">>;
                 'false' -> <<"http">>
             end,
    list_to_binary([Scheme, "://", wh_util:to_binary(H), ":", wh_util:to_binary(P), "/"]);
build_url(H, P, User, Pwd) ->
    Scheme = case ?USE_HTTPS of
                 'true' -> <<"https">>;
                 'false' -> <<"http">>
             end,
    list_to_binary([Scheme, "://", wh_util:to_binary(User), ":", wh_util:to_binary(Pwd)
                    ,"@", wh_util:to_binary(H), ":", wh_util:to_binary(P), "/"
                   ]).

convert_stream_type(<<"extant">>) -> <<"continuous">>;
convert_stream_type(<<"store">>) -> <<"store">>;
convert_stream_type(_) -> <<"single">>.

-spec media_path(api_binary()) -> api_binary().
-spec media_path(api_binary(), api_binary() | whapps_call:call()) -> api_binary().
media_path(Path) -> media_path(Path, 'undefined').

media_path('undefined', _AccountId) -> 'undefined';
media_path(<<"/system_media", _/binary>> = Path, _AccountId) -> Path;
media_path(<<"system_media", _/binary>> = Path, _AccountId) -> Path;
media_path(<<"local_stream://",_/binary>> = Path, _AccountId) -> Path;
media_path(<<"silence_stream://",_/binary>> = Path, _AccountId) -> Path;
media_path(_Path, 'undefined') -> 'undefined';
media_path(Path, AccountId) when is_binary(AccountId) ->
    case binary:match(Path, <<"/">>) of
        'nomatch' -> <<$/, AccountId/binary, $/, Path/binary>>;
        _Else -> Path
    end;
media_path(Path, Call) ->
    media_path(Path, whapps_call:account_id(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prompt_path(ne_binary()) -> ne_binary().
-spec prompt_path(ne_binary(), ne_binary()) -> ne_binary().
prompt_path(PromptId) ->
    prompt_path(?WH_MEDIA_DB, PromptId).

prompt_path(Db, PromptId) ->
    wh_util:join_binary([<<>>, Db, PromptId], <<"/">>).

-spec prompt_id(ne_binary()) -> ne_binary().
-spec prompt_id(ne_binary(), api_binary()) -> ne_binary().
prompt_id(PromptId) -> prompt_id(PromptId, 'undefined').

prompt_id(PromptId, 'undefined') -> PromptId;
prompt_id(PromptId, <<>>) -> PromptId;
prompt_id(PromptId, Lang) ->
    cow_qs:urlencode(<<Lang/binary, "/", PromptId/binary>>).

-spec get_prompt(ne_binary()) -> api_binary().
-spec get_prompt(ne_binary(), api_binary() | whapps_call:call()) ->
                        api_binary().
-spec get_prompt(ne_binary(), api_binary(), api_binary() | whapps_call:call()) ->
                        api_binary().

get_prompt(Name) ->
    get_prompt(Name, 'undefined').

get_prompt(Name, 'undefined') ->
    get_prompt(Name, default_prompt_language(), 'undefined');
get_prompt(Name, <<_/binary>> = Lang) ->
    get_prompt(Name, Lang, 'undefined');
get_prompt(Name, Call) ->
    get_prompt(Name
               ,whapps_call:language(Call)
               ,Call
              ).

get_prompt(Name, Lang, 'undefined') ->
    get_system_prompt(Name, Lang);
get_prompt(Name, 'undefined', Call) ->
    get_prompt(Name, prompt_language(whapps_call:account_id(Call)), Call);
get_prompt(Name, Lang, Call) ->
    get_account_prompt(Name, Lang, Call).

%% tries account default, then system
get_account_prompt(Name, 'undefined', Call) ->
    PromptId = prompt_id(Name),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(whapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_prompt(Name, prompt_language(whapps_call:account_id(Call)), 'undefined');
        {'ok', _} -> prompt_path(whapps_call:account_id(Call), PromptId)
    end;
%% Tries "en", "fr", etc, then fails to default account/system prompt
get_account_prompt(Name, <<_Primary:2/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(whapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call);
        {'ok', _} -> prompt_path(whapps_call:account_id(Call), PromptId)
    end;
%% First tries "en-US" or "fr-CA", etc, then tries "en", "fr", etc.
get_account_prompt(Name, <<Primary:2/binary, "-", _SubTag:2/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(whapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, Call);
        {'ok', _} -> prompt_path(whapps_call:account_id(Call), PromptId)
    end;
%% First tries "en-us_fr-fr", then "en-us"
get_account_prompt(Name, <<Primary:5/binary, "_", _Secondary:5/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(whapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, Call);
        {'ok', _} -> prompt_path(whapps_call:account_id(Call), PromptId)
    end;
%% Matches anything else, then tries account default
get_account_prompt(Name, Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(whapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call);
        {'ok', _} -> prompt_path(whapps_call:account_id(Call), PromptId)
    end.

-spec lookup_prompt(ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', 'not_found'}.
lookup_prompt(Db, Id) ->
    couch_mgr:open_cache_doc(Db, Id).

get_system_prompt(Name, 'undefined') ->
    PromptId = prompt_id(Name),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?WH_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', _} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Lang:2/binary>>) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?WH_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, 'undefined');
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Primary:2/binary, "-", _SubTag:2/binary>> = Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?WH_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, Primary);
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Primary:5/binary, "_", _Secondary:5/binary>> = Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?WH_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, Primary);
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?WH_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, 'undefined');
        {'ok', _Prompt} -> prompt_path(PromptId)
    end.

-spec default_prompt_language() -> ne_binary().
-spec default_prompt_language(api_binary()) -> ne_binary().
default_prompt_language() ->
    default_prompt_language(<<"en-us">>).
default_prompt_language(Default) ->
    wh_util:to_lower_binary(
      whapps_config:get(?WHS_CONFIG_CAT, <<"default_language">>, Default)
     ).

-spec prompt_language(api_binary(), ne_binary()) -> ne_binary().
prompt_language('undefined') -> default_prompt_language();
prompt_language(<<_/binary>> = AccountId) ->
    wh_util:to_lower_binary(
      whapps_account_config:get(AccountId, ?WHS_CONFIG_CAT, <<"default_language">>, default_prompt_language())
     ).

prompt_language('undefined', Default) ->
    default_prompt_language(Default);
prompt_language(<<_/binary>> = AccountId, Default) ->
    wh_util:to_lower_binary(
      whapps_account_config:get(AccountId, ?WHS_CONFIG_CAT, <<"default_language">>, wh_util:to_lower_binary(Default))
     ).
