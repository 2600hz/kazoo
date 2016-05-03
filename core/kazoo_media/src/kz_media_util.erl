%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_media_util).


-export([recording_url/2]).
-export([base_url/2, base_url/3]).
-export([convert_stream_type/1
         ,normalize_media/3
        ]).
-export([media_path/1, media_path/2]).
-export([max_recording_time_limit/0]).
-export([get_prompt/1, get_prompt/2, get_prompt/3
         ,get_account_prompt/3, get_system_prompt/2
         ,default_prompt_language/0, default_prompt_language/1
         ,prompt_language/1, prompt_language/2
         ,prompt_id/1, prompt_id/2
         ,prompt_path/1, prompt_path/2
        ]).
-export([store_path_from_doc/1, store_path_from_doc/2]).

-include("kazoo_media.hrl").

-define(USE_HTTPS, kapps_config:get_is_true(?CONFIG_CAT, <<"use_https">>, 'false')).
-define(AUTH_PLAYBACK, kapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_playback">>, 'false')).
-define(AUTH_USERNAME, kapps_config:get_string(?CONFIG_CAT, <<"proxy_username">>, kz_util:rand_hex_binary(8))).
-define(AUTH_PASSWORD, kapps_config:get_string(?CONFIG_CAT, <<"proxy_password">>, kz_util:rand_hex_binary(8))).
-define(USE_AUTH_STORE, kapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_store">>, 'true')).

-define(NORMALIZE_EXE, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_executable">>, <<"sox">>)).
-define(NORMALIZE_SOURCE_ARGS, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_source_args">>, <<>>)).
-define(NORMALIZE_DEST_ARGS, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_destination_args">>, <<"-r 8000">>)).

-define(USE_ACCOUNT_OVERRIDES, kapps_config:get_is_true(?CONFIG_CAT, <<"support_account_overrides">>, 'true')).

-type normalized_media() :: {'ok', iolist()} |
                            {'error', ne_binary()}.
-spec normalize_media(ne_binary(), ne_binary(), binary()) ->
                             normalized_media().
-spec normalize_media(ne_binary(), ne_binary(), binary(), kz_proplist()) ->
                             normalized_media().
normalize_media(FromFormat, FromFormat, FileContents) ->
    {'ok', FileContents};
normalize_media(FromFormat, ToFormat, FileContents) ->
    normalize_media(FromFormat, ToFormat, FileContents, []).

normalize_media(FromFormat, ToFormat, FileContents, Options) ->
    OldFlag = process_flag('trap_exit', 'true'),

    FromArgs = props:get_value('from_args', Options, ?NORMALIZE_SOURCE_ARGS),
    ToArgs = props:get_value('to_args', Options, ?NORMALIZE_DEST_ARGS),
    ExtraArgs = props:get_value('extra_args', Options, ""),

    Command = iolist_to_binary([?NORMALIZE_EXE
                                ," ", ExtraArgs
                                ," -t ", FromFormat, " ", FromArgs, " - "
                                ," -t ", ToFormat, " ", ToArgs, " - "
                               ]),
    PortOptions = ['binary'
                   ,'exit_status'
                   ,'use_stdio'
                   ,'stderr_to_stdout'
                  ],
    Response =
        try open_port({'spawn', kz_util:to_list(Command)}, PortOptions) of
            Port ->
                lager:debug("opened port ~p to sox with '~s'", [Port, Command]),
                do_normalize_media(FileContents, Port, props:get_integer_value('timeout', Options, 20 * ?MILLISECONDS_IN_SECOND))
        catch
            _E:_R ->
                lager:debug("failed to open port with '~s': ~s: ~p", [Command, _E, _R]),
                {'error', <<"failed to open conversion utility">>}
        end,
    process_flag('trap_exit', OldFlag),
    Response.

-spec do_normalize_media(binary(), port(), pos_integer()) ->
                             normalized_media().
do_normalize_media(FileContents, Port, Timeout) ->
    try erlang:port_command(Port, FileContents) of
        'true' ->
            lager:debug("sent data to port"),
            wait_for_results(Port, Timeout)
    catch
        _E:_R ->
            lager:debug("failed to send data to port: ~s: ~p", [_E, _R]),
            catch erlang:port_close(Port),
            {'error', <<"failed to communicate with conversion utility">>}
    end.

-spec wait_for_results(port(), pos_integer()) ->  normalized_media().
-spec wait_for_results(port(), pos_integer(), iolist()) -> normalized_media().
wait_for_results(Port, Timeout) ->
    wait_for_results(Port, Timeout, []).
wait_for_results(Port, Timeout, Response) ->
    receive
        {Port, {'data', Msg}} ->
            wait_for_results(Port, Timeout, [Response | [Msg]]);
        {Port, {'exit_status', 0}} ->
            lager:debug("process exited successfully"),
            {'ok', Response};
        {Port, {'exit_status', _Err}} ->
            lager:debug("process exited with status ~p", [_Err]),
            {'error', iolist_to_binary(Response)};
        {'EXIT', Port, _Reason} ->
            lager:debug("port closed unexpectedly: ~p", [_Reason]),
            {'error', iolist_to_binary(Response)}
    after Timeout ->
            lager:debug("timeout, sending error response: ~p", [Response]),
            catch erlang:port_close(Port),
            {'error', iolist_to_binary(Response)}
    end.

-spec recording_url(ne_binary(), kz_json:object()) -> ne_binary().
recording_url(CallId, Data) ->
    %% TODO: look at the URL for {filename}, if present replace it with
    %%   the filename, otherwise continue to append as we do today.
    %%   If this is updated be sure and fix the similar code in ecallmgr_call_events!
    Format = kz_json:get_value(<<"format">>, Data, <<"mp3">>),
    Url = kz_util:strip_right_binary(kz_json:get_value(<<"url">>, Data, <<>>), $/),
    <<Url/binary, "/call_recording_", CallId/binary, ".", Format/binary>>.

-spec max_recording_time_limit() -> ?SECONDS_IN_HOUR.
max_recording_time_limit() ->
    kapps_config:get_integer(?WHM_CONFIG_CAT, <<"max_recording_time_limit">>, ?SECONDS_IN_HOUR).

%% base_url(Host) ->
%%     Port = kz_couch_connections:get_port(),
%%     base_url(Host, Port).

base_url(Host, Port) ->
    base_url(Host, Port, 'proxy_playback').

%% base_url(Host, Port, 'direct_playback') ->
%%     case ?AUTH_PLAYBACK of
%%         'false' -> build_url(Host, Port, [], []);
%%         'true' ->
%%             {Username, Password} = kz_couch_connections:get_creds(),
%%             build_url(Host, Port, Username, Password)
%%     end;
base_url(Host, Port, 'proxy_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, [], []);
        'true' -> build_url(Host, Port, ?AUTH_USERNAME, ?AUTH_PASSWORD)
    end;
%% base_url(Host, Port, 'direct_store') ->
%%     case ?USE_AUTH_STORE of
%%         'false' -> build_url(Host, Port, [], []);
%%         'true' ->
%%             {Username, Password} = kz_couch_connections:get_creds(),
%%             build_url(Host, Port, Username, Password)
%%     end;
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
    list_to_binary([Scheme, "://", kz_util:to_binary(H), ":", kz_util:to_binary(P), "/"]);
build_url(H, P, User, Pwd) ->
    Scheme = case ?USE_HTTPS of
                 'true' -> <<"https">>;
                 'false' -> <<"http">>
             end,
    list_to_binary([Scheme, "://", kz_util:to_binary(User), ":", kz_util:to_binary(Pwd)
                    ,"@", kz_util:to_binary(H), ":", kz_util:to_binary(P), "/"
                   ]).

convert_stream_type(<<"extant">>) -> <<"continuous">>;
convert_stream_type(<<"store">>) -> <<"store">>;
convert_stream_type(_) -> <<"single">>.

-spec media_path(api_binary()) -> api_binary().
-spec media_path(api_binary(), api_binary() | kapps_call:call()) -> api_binary().
media_path(Path) -> media_path(Path, 'undefined').

media_path('undefined', _AccountId) -> 'undefined';
media_path(<<>>, _AccountId) -> 'undefined';
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
    media_path(Path, kapps_call:account_id(Call)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prompt_path(ne_binary()) -> ne_binary().
-spec prompt_path(api_binary(), ne_binary()) -> ne_binary().
prompt_path(PromptId) ->
    prompt_path(?KZ_MEDIA_DB, PromptId).

prompt_path('undefined', PromptId) ->
    prompt_path(?KZ_MEDIA_DB, PromptId);
prompt_path(Db, <<"/system_media/", PromptId/binary>>) ->
    prompt_path(Db, PromptId);
prompt_path(Db, PromptId) ->
    kz_util:join_binary([<<>>, Db, PromptId], <<"/">>).

-spec prompt_id(ne_binary()) -> ne_binary().
-spec prompt_id(ne_binary(), api_binary()) -> ne_binary().
prompt_id(PromptId) -> prompt_id(PromptId, 'undefined').

prompt_id(<<"/system_media/", PromptId/binary>>, Lang) ->
    prompt_id(PromptId, Lang);
prompt_id(PromptId, 'undefined') -> PromptId;
prompt_id(PromptId, <<>>) -> PromptId;
prompt_id(PromptId, Lang) ->
    <<Lang/binary, "/", PromptId/binary>>.

-spec get_prompt(ne_binary()) -> api_binary().
-spec get_prompt(ne_binary(), api_binary() | kapps_call:call()) ->
                        api_binary().
-spec get_prompt(ne_binary(), api_binary(), api_binary() | kapps_call:call()) ->
                        api_binary().
-spec get_prompt(ne_binary(), api_binary(), api_binary(), boolean()) -> api_binary().

get_prompt(Name) ->
    get_prompt(Name, 'undefined').

get_prompt(Name, 'undefined') ->
    get_prompt(Name, default_prompt_language(), 'undefined');
get_prompt(Name, <<_/binary>> = Lang) ->
    get_prompt(Name, Lang, 'undefined');
get_prompt(Name, Call) ->
    Lang = kapps_call:language(Call),
    get_prompt(Name, Lang, Call).

get_prompt(<<"prompt://", _/binary>> = PromptId, _Lang, _Call) ->
    lager:debug("prompt is already encoded: ~s", [PromptId]),
    PromptId;
get_prompt(<<"/system_media/", Name/binary>>, Lang, Call) ->
    get_prompt(Name, Lang, Call);
get_prompt(PromptId, Lang, 'undefined') ->
    kz_util:join_binary([<<"prompt:/">>, ?KZ_MEDIA_DB, PromptId, Lang], <<"/">>);
get_prompt(PromptId, Lang, <<_/binary>> = AccountId) ->
    get_prompt(PromptId, Lang, AccountId, ?USE_ACCOUNT_OVERRIDES);
get_prompt(PromptId, Lang, Call) ->
    get_prompt(PromptId, Lang, kapps_call:account_id(Call)).

get_prompt(<<"prompt://", _/binary>> = PromptId, _Lang, _AccountId, _UseOverride) ->
    lager:debug("prompt is already encoded: ~s", [PromptId]),
    PromptId;
get_prompt(PromptId, Lang, AccountId, 'true') ->
    lager:debug("using account override for ~s in account ~s", [PromptId, AccountId]),
    kz_util:join_binary([<<"prompt:/">>, AccountId, PromptId, Lang], <<"/">>);
get_prompt(PromptId, Lang, _AccountId, 'false') ->
    lager:debug("account overrides not enabled; ignoring account prompt for ~s", [PromptId]),
    kz_util:join_binary([<<"prompt:/">>, ?KZ_MEDIA_DB, PromptId, Lang], <<"/">>).

-spec get_account_prompt(ne_binary(), api_binary(), kapps_call:call()) -> api_binary().
-spec get_account_prompt(ne_binary(), api_binary(), kapps_call:call(), ne_binary()) -> api_binary().
%% tries account default, then system
get_account_prompt(Name, 'undefined', Call) ->
    PromptId = prompt_id(Name),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_prompt(Name, prompt_language(kapps_call:account_id(Call)), 'undefined');
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
%% Tries "en", "fr", etc, then fails to default account/system prompt
get_account_prompt(Name, <<_Primary:2/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call, Lang);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
%% First tries "en-US" or "fr-CA", etc, then tries "en", "fr", etc.
get_account_prompt(Name, <<Primary:2/binary, "-", _SubTag:2/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, Call, Lang);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
%% First tries "en-us_fr-fr", then "en-us"
get_account_prompt(Name, <<Primary:5/binary, "_", _Secondary:5/binary>> = Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, Call, Lang);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
%% Matches anything else, then tries account default
get_account_prompt(Name, Lang, Call) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end.

get_account_prompt(Name, 'undefined', Call, OriginalLang) ->
    PromptId = prompt_id(Name),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_prompt(Name, OriginalLang, 'undefined');
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
get_account_prompt(Name, <<_:2/binary>> = Primary, Call, Original) ->
    PromptId = prompt_id(Name, Primary),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call, Original);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
get_account_prompt(Name, <<Primary:2/binary, "-", _Secondary:2/binary>> = Lang, Call, Original) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, Call, Original);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end;
get_account_prompt(Name, Lang, Call, OriginalLang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kapps_call:account_db(Call), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', Call, OriginalLang);
        {'ok', _} -> prompt_path(kapps_call:account_id(Call), PromptId)
    end.

-spec lookup_prompt(ne_binary(), ne_binary()) ->
                           {'ok', kz_json:object()} |
                           {'error', 'not_found'}.
lookup_prompt(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, Id) of
        {'ok', Doc} ->
            prompt_is_usable(Doc);
        Error -> Error
    end.

-spec prompt_is_usable(kz_json:object()) ->
                              {'ok', kz_json:object()} |
                              {'error', 'not_found'}.
prompt_is_usable(Doc) ->
    case kz_doc:is_soft_deleted(Doc) of
        'true' -> {'error', 'not_found'};
        'false' -> {'ok', Doc}
    end.

-spec get_system_prompt(ne_binary(), api_binary()) -> api_binary().
get_system_prompt(Name, 'undefined') ->
    PromptId = prompt_id(Name),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?KZ_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> 'undefined';
        {'ok', _} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Lang:2/binary>>) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?KZ_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, 'undefined');
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Primary:2/binary, "-", _SubTag:2/binary>> = Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?KZ_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, Primary);
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, <<Primary:5/binary, "_", _Secondary:5/binary>> = Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?KZ_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, Primary);
        {'ok', _Prompt} -> prompt_path(PromptId)
    end;
get_system_prompt(Name, Lang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting system prompt for '~s'", [PromptId]),

    case lookup_prompt(?KZ_MEDIA_DB, PromptId) of
        {'error', 'not_found'} -> get_system_prompt(Name, 'undefined');
        {'ok', _Prompt} -> prompt_path(PromptId)
    end.

-spec default_prompt_language() -> ne_binary().
-spec default_prompt_language(api_binary()) -> ne_binary().
default_prompt_language() ->
    default_prompt_language(<<"en-us">>).
default_prompt_language(Default) ->
    kz_util:to_lower_binary(
      kapps_config:get(?WHM_CONFIG_CAT, ?PROMPT_LANGUAGE_KEY, Default)
     ).

-spec prompt_language(api_binary()) -> ne_binary().
prompt_language(?KZ_MEDIA_DB) -> default_prompt_language();
prompt_language(AccountId) ->
    prompt_language(AccountId, default_prompt_language()).

-spec prompt_language(api_binary(), api_binary()) -> ne_binary().
prompt_language('undefined', Default) ->
    default_prompt_language(Default);
prompt_language(<<_/binary>> = AccountId, 'undefined') ->
    prompt_language(AccountId);
prompt_language(<<_/binary>> = AccountId, Default) ->
    case ?USE_ACCOUNT_OVERRIDES of
        'false' -> default_prompt_language();
        'true' ->
            kz_util:to_lower_binary(
              kapps_account_config:get(AccountId, ?WHM_CONFIG_CAT, ?PROMPT_LANGUAGE_KEY, kz_util:to_lower_binary(Default))
             )
    end.

-spec store_path_from_doc(kz_json:object()) -> media_store_path() | {'error', 'no_attachments'}.
store_path_from_doc(JObj) ->
    case kz_doc:attachment_names(JObj) of
        [] -> {'error', 'no_attachments'};
        [AName | _] -> store_path_from_doc(JObj, AName)
    end.

-spec store_path_from_doc(kz_json:object(), ne_binary()) -> media_store_path().
store_path_from_doc(JObj, AName) ->
    Opts = [{'doc_type', kz_doc:type(JObj)}
            ,{'doc_owner', kz_json:get_value(<<"owner_id">>, JObj)}
            ,{'storage_id', kz_json:get_first_defined([<<"storage_ref_id">>, <<"source_id">>], JObj)}
           ],
    #media_store_path{db = kz_doc:account_db(JObj)
                     ,id = kz_doc:id(JObj)
                     ,att = AName
                     ,opt = props:filter_undefined(Opts)
                     }.
