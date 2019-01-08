%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_util).

-export([recording_url/2]).
-export([base_url/2, base_url/3
        ,proxy_host/0
        ,proxy_port/0
        ,proxy_base_url/0, proxy_base_url/1
        ]).
-export([convert_stream_type/1
        ,normalize_media/3, normalize_media/4
        ,normalize_media_file/3, normalize_media_file/4
        ,synthesize_tone/3, synthesize_tone/4
        ,detect_file_sample_rate/1
        ,detect_file_format/1
        ,join_media_files/1, join_media_files/2
        ]).
-export([media_path/1, media_path/2]).
-export([max_recording_time_limit/0]).
-export([get_prompt/1, get_prompt/2, get_prompt/3
        ,get_account_prompt/3, get_system_prompt/2
        ,default_prompt_language/0, default_prompt_language/1
        ,prompt_language/1, prompt_language/2
        ,prompt_id/1, prompt_id/2
        ,prompt_path/1, prompt_path/2
        ,is_not_prompt/1
        ]).
-export([store_path_from_doc/1, store_path_from_doc/2]).

-export_type([normalized_media/0
             ,normalization_options/0
             ]).

-include("kazoo_media.hrl").

-define(USE_HTTPS, kapps_config:get_is_true(?CONFIG_CAT, <<"use_https">>, 'false')).
-define(AUTH_PLAYBACK, kapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_playback">>, 'false')).
-define(AUTH_USERNAME, kapps_config:get_binary(?CONFIG_CAT, <<"proxy_username">>, kz_binary:rand_hex(8))).
-define(AUTH_PASSWORD, kapps_config:get_binary(?CONFIG_CAT, <<"proxy_password">>, kz_binary:rand_hex(8))).
-define(USE_AUTH_STORE, kapps_config:get_is_true(?CONFIG_CAT, <<"authenticated_store">>, 'true')).

-define(NORMALIZE_EXE, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_executable">>, <<"sox">>)).
-define(NORMALIZE_SOURCE_ARGS, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_source_args">>, <<>>)).
-define(NORMALIZE_DEST_ARGS, kapps_config:get_binary(?CONFIG_CAT, <<"normalize_destination_args">>, <<"-r 8000">>)).

-define(NORMALIZATION_FORMAT, kapps_config:get_ne_binary(<<"crossbar.media">>, <<"normalization_format">>, <<"mp3">>)).

-define(USE_ACCOUNT_OVERRIDES, kapps_config:get_is_true(?CONFIG_CAT, <<"support_account_overrides">>, 'true')).

%%------------------------------------------------------------------------------
%% @doc Normalize audio file to the system default or specified sample rate.
%% Accepts media file content binary as input.
%%
%% By default it returns result as binary, if you want file path to the
%%  normalized file only, pass the `{output, file}' as option.
%% @end
%%------------------------------------------------------------------------------
-type normalized_media() :: {'ok', binary()} |
                            {'ok', file:filename_all()} |
                            {'error', file:posix()}.

-type normalization_option() :: {'extra_args', iodata()} |
                                {'from_args', binary()} |
                                {'out_file', file:filename_all()} |
                                {'output', 'binary' | 'file'} |
                                {'to_args', binary()}.
-type normalization_options() :: [normalization_option()].

-type proxy_scheme() :: 'http' | 'https'.

-spec normalize_media(kz_term:ne_binary(), kz_term:ne_binary(), binary()) ->
                             normalized_media().
normalize_media(FromFormat, FromFormat, FileContents) ->
    {'ok', FileContents};
normalize_media(FromFormat, ToFormat, FileContents) ->
    normalize_media(FromFormat, ToFormat, FileContents, default_normalization_options(ToFormat)).

-spec normalize_media(kz_term:ne_binary(), kz_term:ne_binary(), binary(), normalization_options()) ->
                             normalized_media().
normalize_media(FromFormat, ToFormat, FileContents, Options) ->
    FileName = tmp_file(FromFormat),
    case file:write_file(FileName, FileContents) of
        'ok' ->
            Result = normalize_media_file(FromFormat, ToFormat, FileName, Options),
            kz_util:delete_file(FileName),
            Result;
        {'error', _}=Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Normalize audio file to the system default or specified sample rate.
%% Accept a path to the media file as input.
%%
%% By default it returns result as binary, if you want file path to the
%%  normalized file only, pass the {'output', 'file'} as option.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_media_file(kz_term:ne_binary(), kz_term:ne_binary(), file:filename_all()) ->
                                  normalized_media().
normalize_media_file(FromFormat, FromFormat, FromFile) ->
    {'ok', FromFile};
normalize_media_file(FromFormat, ToFormat, FromFile) ->
    normalize_media_file(FromFormat, ToFormat, FromFile, default_normalization_options(ToFormat)).

-spec default_normalization_options(kz_term:ne_binary()) -> normalization_options().
default_normalization_options(ToFormat) ->
    [{'from_args', ?NORMALIZE_SOURCE_ARGS}
    ,{'to_args', ?NORMALIZE_DEST_ARGS}
    ,{'extra_args', ""}
    ,{'out_file', tmp_file(ToFormat)}
    ,{'output', 'binary'}
    ].

-spec normalize_media_file(kz_term:ne_binary(), kz_term:ne_binary(), file:filename_all(), normalization_options()) ->
                                  normalized_media().
normalize_media_file(FromFormat, ToFormat, FromFile, Options) ->
    FromArgs = props:get_value('from_args', Options, ?NORMALIZE_SOURCE_ARGS),
    ToArgs = props:get_value('to_args', Options, ?NORMALIZE_DEST_ARGS),
    ExtraArgs = props:get_value('extra_args', Options, ""),
    ToFile = props:get_value('out_file', Options, tmp_file(ToFormat)),

    Command = iolist_to_binary([?NORMALIZE_EXE
                               ," ", ExtraArgs
                               ," -t ", FromFormat, " ", FromArgs, " ", FromFile
                               ," -t ", ToFormat, " ", ToArgs, " ", ToFile
                               ]),
    OutputType = props:get_value('output', Options, 'binary'),
    lager:debug("normalize media with command: ~p", [Command]),
    return_command_result(run_command(Command), ToFile, OutputType).

-spec return_command_result({'ok', any()} | {'error', any()}, file:filename_all(), 'binary' | 'file') ->
                                   normalized_media().
return_command_result({'ok', _}, FileName, 'binary') ->
    case file:read_file(FileName) of
        {'ok', _}=OK ->
            _ = kz_util:delete_file(FileName),
            OK;
        {'error', _}=Error -> Error
    end;
return_command_result({'ok', _}, FileName, 'file') -> {'ok', FileName};
return_command_result({'error', _}=Error, _FileName, _) -> Error.

%%------------------------------------------------------------------------------
%% @doc Run normalizer command
%% @end
%%------------------------------------------------------------------------------
-spec run_command(kz_term:ne_binary()) -> normalized_media().
run_command(Command) ->
    try os:cmd(binary_to_list(Command)) of
        Result ->
            lager:debug("conversion utility result: ~p", [Result]),
            {'ok', Result}
    catch
        _E:Reason ->
            lager:warning("conversion utility resulted in an exception: ~p:~p", [_E, Reason]),
            {'error', Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Synthesize a tone, returns as binary or a path to the generated file
%% @end
%%------------------------------------------------------------------------------
-spec synthesize_tone(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> normalized_media().
synthesize_tone(SampleRate, Frequency, Length) ->
    FileName = tmp_file(<<"wav">>),
    case synthesize_tone(SampleRate, Frequency, Length, FileName) of
        {'ok', _} ->
            Result = file:read_file(FileName),
            kz_util:delete_file(FileName),
            Result;
        {'error', _}=Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Synthesize a tone, returns as binary or a path to the generated file
%% @end
%%------------------------------------------------------------------------------
-spec synthesize_tone(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> normalized_media().
synthesize_tone(SampleRate, Frequency, Length, FileName) ->
    Command = iolist_to_binary([?NORMALIZE_EXE
                               ,<<" -r ">>, SampleRate
                               ,<<" -n ">>, FileName
                               ,<<" synth ">>, Length
                               ,<<" sin ">>, Frequency
                               ]),
    lager:info("synthesize tone command ~p", [Command]),
    run_command(Command).

%%------------------------------------------------------------------------------
%% @doc Detect sample rate of a media file
%% @end
%%------------------------------------------------------------------------------
-spec detect_file_sample_rate(kz_term:ne_binary()) -> normalized_media().
detect_file_sample_rate(FileName) ->
    Command = iolist_to_binary([?NORMALIZE_EXE
                               ,<<" --i -r ">>
                               ,FileName
                               ]),
    lager:info("detect sample rate command ~p", [Command]),
    case run_command(Command) of
        {'ok', Result} ->
            Regex = "^(\\d+).*$",
            RegexOptions = [{'capture', [1], 'binary'}],
            case re:run(Result, Regex, RegexOptions) of
                {'match', [SampleRate]} ->
                    {'ok', SampleRate};
                _Else ->
                    {'error', 'detection_failed'}
            end;
        {'error', _} ->
            {'error', 'detection_failed'}
    end.

%%------------------------------------------------------------------------------
%% @doc Detect media format of a media file
%% @end
%%------------------------------------------------------------------------------
-spec detect_file_format(kz_term:ne_binary()) -> normalized_media().
detect_file_format(FileName) ->
    Command = iolist_to_binary([?NORMALIZE_EXE
                               ,<<" --i -t ">>
                               ,FileName
                               ]),
    lager:info("detect file type command ~p~n~n", [Command]),
    case run_command(Command) of
        {'ok', Result} ->
            Regex = "^(\\w+).*$",
            RegexOptions = [{'capture', [1], 'binary'}],
            case re:run(Result, Regex, RegexOptions) of
                {'match', [Format]} ->
                    {'ok', Format};
                _Else ->
                    {'error', 'detection_failed'}
            end;
        {'error', _} ->
            {'error', 'detection_failed'}
    end.

%%------------------------------------------------------------------------------
%% @doc Join multiple Audio file together. It detects file format, sample rate
%% of each file and will try to convert sample rate if it different from the
%% requested sample rate or the default value of 16kHz.
%% @end
%%------------------------------------------------------------------------------
-type join_file() :: {kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()}.
-type join_files() :: [join_file()].

-spec join_media_files(kz_term:ne_binaries()) -> normalized_media().
join_media_files(FileNames) ->
    join_media_files(FileNames, []).

-spec join_media_files(kz_term:ne_binaries(), kz_term:proplist()) -> normalized_media().
join_media_files(FileNames, Options) ->
    DetectedOpts = [detect_format_options(F) || F <- FileNames],
    SampleRate = props:get_value('sample_rate', Options, <<"16000">>),
    case maybe_normalize_copy_files(DetectedOpts, SampleRate, []) of
        {'ok', NewOpts} ->
            maybe_join_media_files(NewOpts, Options, []);
        {'error', _}=Error -> Error
    end.

-spec maybe_join_media_files(join_files(), kz_term:proplist(), join_files()) -> normalized_media().
maybe_join_media_files([], Options, Acc) -> do_join_media_files(Acc, Options);
maybe_join_media_files([{_, 'undefined', _}|_], _, _) -> {'error', 'join_media_failed'};
maybe_join_media_files([{_, _, 'undefined'}|_], _, _) -> {'error', 'join_media_failed'};
maybe_join_media_files([F|Files], Options, Acc) ->
    maybe_join_media_files(Files, Options, [F|Acc]).

-spec do_join_media_files(join_files(), kz_term:proplist()) -> normalized_media().
do_join_media_files(Files, Options) ->
    SampleRate = props:get_value('sample_rate', Options, <<"16000">>),
    ToFormat = props:get_value('to_format', Options, ?NORMALIZATION_FORMAT),
    ToFile = props:get_value('out_file', Options, tmp_file(ToFormat)),
    Command = iolist_to_binary(
                [?NORMALIZE_EXE
                ,[[" -r ", Rate, " -t ", Format, " ", File]
                  || {File, Rate, Format} <- Files
                 ]
                ," -r ", SampleRate
                ," -t ", ToFormat
                ," ", ToFile
                ]
               ),
    OutputType = props:get_value('output', Options, 'binary'),
    lager:debug("joining media files with command: ~p", [Command]),
    Result = return_command_result(run_command(Command), ToFile, OutputType),
    %% cleanup copied files
    _ = [kz_util:delete_file(F) || {F, _, _} <- Files],
    Result.

%%------------------------------------------------------------------------------
%% @doc Based on detected format options, normalize the files if
%% sample rate are different of desire sample rate, otherwise
%% copy the files to a temporary place to join them together.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_normalize_copy_files(join_files(), kz_term:ne_binary(), join_files()) ->
                                        {'ok', join_files()} |
                                        {'error', 'normalization_failed'}.
maybe_normalize_copy_files([], _SampleRate, Acc) -> {'ok', Acc};
maybe_normalize_copy_files([{File, SampleRate, Format}|Files], SampleRate, Acc) ->
    NewFile = tmp_file(Format),
    case file:copy(File, NewFile) of
        {'ok', _} ->
            maybe_normalize_copy_files(Files, SampleRate, [{NewFile, SampleRate, Format}|Acc]);
        {'error', _Reason} ->
            lager:warning("failed to copy file to a temporary place to join media files: ~p", [_Reason]),
            %% cleanup already copied files
            _ = [kz_util:delete_file(F) || {F, _, _} <- Files],
            {'error', 'normalization_failed'}
    end;
maybe_normalize_copy_files([{File, _Other, Format}|Files], SampleRate, Acc) ->
    NormOptions = [{'output', 'file'}
                  ,{'to_args', <<"-r ", SampleRate/binary>>}
                  ],
    case normalize_media_file(Format, Format, File, NormOptions) of
        {'ok', NormFile} ->
            maybe_normalize_copy_files(Files, SampleRate, [{NormFile, SampleRate, Format}|Acc]);
        {'error', _Reason} ->
            lager:warning("can't normalize file ~s for preforming join media: ~p", [File, _Reason]),
            %% cleanup already copied files
            _ = [kz_util:delete_file(F) || {F, _, _} <- Files],
            {'error', 'normalization_failed'}
    end.

%%------------------------------------------------------------------------------
%% @doc Detect file format options(sample_rate, file format) and return
%% a tuple of detect options
%% @end
%%------------------------------------------------------------------------------
-spec detect_format_options(kz_term:ne_binary()) -> join_file().
detect_format_options(File) ->
    FileSampleRate = detect_file_sample_rate(File),
    FileFormat = detect_file_format(File),
    case {FileSampleRate, FileFormat} of
        {{'ok', SampleRate}, {'ok', Format}} ->
            {File, SampleRate, Format};
        {{'ok', SampleRate}, {'error', _}} ->
            {File, SampleRate, 'undefined'};
        {{'error', _}, {'ok', Format}} ->
            {File, 'undefined', Format};
        {{'error', _}, {'error', _}} ->
            {File, 'undefined', 'undefined'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec tmp_file(kz_term:ne_binary()) -> file:filename_all().
tmp_file(Ext) ->
    filename:join(["/tmp", list_to_binary([kz_binary:rand_hex(16), ".", Ext])]).

-spec recording_url(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary().
recording_url(CallId, Data) ->
    %% TODO: look at the URL for {filename}, if present replace it with
    %%   the filename, otherwise continue to append as we do today.
    %%   If this is updated be sure and fix the similar code in ecallmgr_call_events!
    Format = kz_json:get_value(<<"format">>, Data, <<"mp3">>),
    Url = kz_binary:strip_right(kz_json:get_value(<<"url">>, Data, <<>>), $/),
    <<Url/binary, "/call_recording_", CallId/binary, ".", Format/binary>>.

-spec max_recording_time_limit() -> ?SECONDS_IN_HOUR.
max_recording_time_limit() ->
    kapps_config:get_integer(?CONFIG_CAT, <<"max_recording_time_limit">>, ?SECONDS_IN_HOUR).

%% base_url(Host) ->
%%     Port = kz_couch_connections:get_port(),
%%     base_url(Host, Port).

-spec base_url(kz_term:text(), inet:port_number() | kz_term:text()) -> kz_term:ne_binary().
base_url(Host, Port) ->
    base_url(Host, Port, 'proxy_playback').

-spec base_url(kz_term:text(), inet:port_number() | kz_term:text(), atom()) -> kz_term:ne_binary().
base_url(Host, Port, 'proxy_playback') ->
    case ?AUTH_PLAYBACK of
        'false' -> build_url(Host, Port, <<>>);
        'true' -> build_url(Host, Port, list_to_binary([?AUTH_USERNAME, ":", ?AUTH_PASSWORD, "@"]))
    end;
base_url(Host, Port, 'proxy_store') ->
    case ?USE_AUTH_STORE of
        'false' -> build_url(Host, Port, <<>>);
        'true' -> build_url(Host, Port, list_to_binary([?AUTH_USERNAME, ":", ?AUTH_PASSWORD, "@"]))
    end.

-spec build_url(kz_term:ne_binary(), inet:port_number(), binary()) -> kz_term:ne_binary().
build_url(H, P, Creds) ->
    build_url(proxy_scheme(), H, P, Creds).

-spec build_url(proxy_scheme(), kz_term:ne_binary(), inet:port_number(), binary()) -> kz_term:ne_binary().
build_url('https', H, 443, Creds) ->
    list_to_binary(["https://", Creds, kz_term:to_binary(H)]);
build_url('https', H, P, Creds) ->
    list_to_binary(["https://", Creds, kz_term:to_binary(H), ":", kz_term:to_binary(P)]);
build_url('http', H, 80, Creds) ->
    list_to_binary(["http://", Creds, kz_term:to_binary(H)]);
build_url('http', H, P, Creds) ->
    list_to_binary(["http://", Creds, kz_term:to_binary(H), ":", kz_term:to_binary(P)]).

-spec convert_stream_type(kz_term:ne_binary()) -> kz_term:ne_binary().
convert_stream_type(<<"extant">>) -> <<"continuous">>;
convert_stream_type(<<"store">>) -> <<"store">>;
convert_stream_type(_) -> <<"single">>.

-spec media_path(kz_term:api_binary()) -> kz_term:api_binary().
media_path(Path) -> media_path(Path, 'undefined').

-spec media_path(kz_term:api_binary(), kz_term:api_ne_binary()) -> kz_term:api_binary().
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
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec prompt_path(kz_term:ne_binary()) -> kz_term:ne_binary().
prompt_path(PromptId) ->
    prompt_path(?KZ_MEDIA_DB, PromptId).

-spec prompt_path(kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
prompt_path('undefined', PromptId) ->
    prompt_path(?KZ_MEDIA_DB, PromptId);
prompt_path(Db, <<"/system_media/", PromptId/binary>>) ->
    prompt_path(Db, PromptId);
prompt_path(Db, PromptId) ->
    kz_binary:join([<<>>, Db, PromptId], <<"/">>).

-spec prompt_id(kz_term:ne_binary()) -> kz_term:ne_binary().
prompt_id(PromptId) -> prompt_id(PromptId, 'undefined').

-spec prompt_id(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
prompt_id(<<"/system_media/", PromptId/binary>>, Lang) ->
    prompt_id(PromptId, Lang);
prompt_id(PromptId, 'undefined') -> PromptId;
prompt_id(PromptId, <<>>) -> PromptId;
prompt_id(PromptId, Lang) ->
    case is_not_prompt(PromptId) of
        'true' ->
            lager:debug("not building a prompt id from ~s", [PromptId]),
            PromptId;
        'false' ->
            filename:join([Lang, PromptId])
    end.

-spec get_prompt(kz_term:ne_binary()) ->
                        kz_term:api_ne_binary().
get_prompt(Name) ->
    get_prompt(Name, 'undefined').

-spec get_prompt(kz_term:ne_binary(), kz_term:api_ne_binary()) ->
                        kz_term:api_ne_binary().
get_prompt(Name, 'undefined') ->
    get_prompt(Name, default_prompt_language(), 'undefined');
get_prompt(Name, <<_/binary>> = Lang) ->
    get_prompt(Name, Lang, 'undefined').

-spec get_prompt(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) ->
                        kz_term:api_ne_binary().
get_prompt(<<"prompt://", _/binary>> = PromptId, _Lang, _AccountId) ->
    lager:debug("prompt is already encoded: ~s", [PromptId]),
    PromptId;
get_prompt(<<"/system_media/", Name/binary>>, Lang, AccountId) ->
    get_prompt(Name, Lang, AccountId);
get_prompt(PromptId, Lang, 'undefined') ->
    case is_not_prompt(PromptId) of
        'true' ->
            lager:debug("media ~s is not a prompt, leaving alone", [PromptId]),
            PromptId;
        'false' ->
            lager:debug("using system prompt for ~s(~s)", [PromptId, Lang]),
            kz_binary:join([<<"prompt:/">>, ?KZ_MEDIA_DB, PromptId, Lang], <<"/">>)
    end;
get_prompt(PromptId, Lang, <<_/binary>> = AccountId) ->
    get_prompt(PromptId, Lang, AccountId, ?USE_ACCOUNT_OVERRIDES).

-spec get_prompt(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), boolean()) ->
                        kz_term:api_ne_binary().
get_prompt(<<"prompt://", _/binary>> = PromptId, _Lang, _AccountId, _UseOverride) ->
    lager:debug("prompt is already encoded: ~s", [PromptId]),
    PromptId;
get_prompt(PromptId, Lang, AccountId, 'true') ->
    case is_not_prompt(PromptId) of
        'true' ->
            lager:debug("media ~s is not a prompt, leaving alone", [PromptId]),
            PromptId;
        'false' ->
            lager:debug("using account override for ~s in account ~s", [PromptId, AccountId]),
            kz_binary:join([<<"prompt:/">>, AccountId, PromptId, Lang], <<"/">>)
    end;
get_prompt(PromptId, Lang, _AccountId, 'false') ->
    case is_not_prompt(PromptId) of
        'true' ->
            lager:debug("media ~s is not a prompt, leaving alone", [PromptId]),
            PromptId;
        'false' ->
            lager:debug("account overrides not enabled; ignoring account prompt for ~s", [PromptId]),
            kz_binary:join([<<"prompt:/">>, ?KZ_MEDIA_DB, PromptId, Lang], <<"/">>)
    end.

-spec is_not_prompt(kz_term:api_binary()) -> boolean().
is_not_prompt('undefined') -> 'true';
is_not_prompt(<<>>) -> 'true';
is_not_prompt(<<"silence">>) -> 'true';
is_not_prompt(<<"silence_stream://", _/binary>>) -> 'true';
is_not_prompt(<<"tone_stream://", _/binary>>) -> 'true';
is_not_prompt(<<"shout://", _/binary>>) -> 'true';
is_not_prompt(<<"local_stream://", _/binary>>) -> 'true';
is_not_prompt(<<"http://", _/binary>>) -> 'true';
is_not_prompt(<<"https://", _/binary>>) -> 'true';
is_not_prompt(?NE_BINARY = _Media) -> 'false'.

%% tries account default, then system

-spec get_account_prompt(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary()) ->
                                kz_term:api_ne_binary().
get_account_prompt(Name, 'undefined', AccountId) ->
    PromptId = prompt_id(Name),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_prompt(Name, prompt_language(AccountId), 'undefined');
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
%% Tries "en", "fr", etc, then fails to default account/system prompt
get_account_prompt(Name, <<_Primary:2/binary>> = Lang, AccountId) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', AccountId, Lang);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
%% First tries "en-US" or "fr-CA", etc, then tries "en", "fr", etc.
get_account_prompt(Name, <<Primary:2/binary, "-", _SubTag:2/binary>> = Lang, AccountId) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, AccountId, Lang);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
%% First tries "en-us_fr-fr", then "en-us"
get_account_prompt(Name, <<Primary:5/binary, "_", _Secondary:5/binary>> = Lang, AccountId) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, AccountId, Lang);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
%% Matches anything else, then tries account default
get_account_prompt(Name, Lang, AccountId) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', AccountId);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end.

-spec get_account_prompt(kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                kz_term:api_ne_binary().
get_account_prompt(Name, 'undefined', AccountId, OriginalLang) ->
    PromptId = prompt_id(Name),
    lager:debug("getting account prompt for '~s'", [PromptId]),
    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_prompt(Name, OriginalLang, 'undefined');
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
get_account_prompt(Name, <<_:2/binary>> = Primary, AccountId, Original) ->
    PromptId = prompt_id(Name, Primary),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', AccountId, Original);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
get_account_prompt(Name, <<Primary:2/binary, "-", _Secondary:2/binary>> = Lang, AccountId, Original) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, Primary, AccountId, Original);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end;
get_account_prompt(Name, Lang, AccountId, OriginalLang) ->
    PromptId = prompt_id(Name, Lang),
    lager:debug("getting account prompt for '~s'", [PromptId]),

    case lookup_prompt(kz_util:format_account_db(AccountId), PromptId) of
        {'error', 'not_found'} -> get_account_prompt(Name, 'undefined', AccountId, OriginalLang);
        {'ok', _} -> prompt_path(AccountId, PromptId)
    end.

-spec lookup_prompt(kz_term:ne_binary(), kz_term:ne_binary()) ->
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

-spec get_system_prompt(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:api_binary().
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

-spec default_prompt_language() -> kz_term:ne_binary().
default_prompt_language() ->
    default_prompt_language(<<"en-us">>).

-spec default_prompt_language(kz_term:api_binary()) -> kz_term:ne_binary().
default_prompt_language(Default) ->
    kz_term:to_lower_binary(
      kapps_config:get_ne_binary(?CONFIG_CAT, ?PROMPT_LANGUAGE_KEY, Default)
     ).

-spec prompt_language(kz_term:api_binary()) -> kz_term:ne_binary().
prompt_language(?KZ_MEDIA_DB) -> default_prompt_language();
prompt_language(AccountId) ->
    prompt_language(AccountId, default_prompt_language()).

-spec prompt_language(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
prompt_language('undefined', Default) ->
    default_prompt_language(Default);
prompt_language(<<_/binary>> = AccountId, 'undefined') ->
    prompt_language(AccountId);
prompt_language(<<_/binary>> = AccountId, SystemDefault) ->
    case ?USE_ACCOUNT_OVERRIDES of
        'false' -> default_prompt_language();
        'true' ->
            {'ok', AccountJObj} = kzd_accounts:fetch(AccountId),
            Default = kzd_accounts:language(AccountJObj, SystemDefault),
            kz_term:to_lower_binary(
              kapps_account_config:get_ne_binary(AccountId
                                                ,?CONFIG_CAT
                                                ,?PROMPT_LANGUAGE_KEY
                                                ,kz_term:to_lower_binary(Default)
                                                )
             )
    end.

-spec store_path_from_doc(kz_json:object()) -> media_store_path() | {'error', 'no_attachments'}.
store_path_from_doc(JObj) ->
    case kz_doc:attachment_names(JObj) of
        [] -> {'error', 'no_attachments'};
        [AName | _] -> store_path_from_doc(JObj, AName)
    end.

-spec store_path_from_doc(kz_json:object(), kz_term:ne_binary()) -> media_store_path().
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

-spec proxy_host() -> kz_term:ne_binary().
proxy_host() ->
    case kapps_config:get_ne_binary(?CONFIG_CAT, <<"proxy_hostname">>) of
        'undefined' -> kz_network_utils:get_hostname();
        ProxyHostname -> ProxyHostname
    end.

-spec proxy_port() -> pos_integer().
proxy_port() ->
    kapps_config:get_integer(?CONFIG_CAT, <<"proxy_port">>, 24517).

-spec proxy_scheme() -> proxy_scheme().
proxy_scheme() ->
    proxy_scheme(?USE_HTTPS).

-spec proxy_scheme(boolean()) -> proxy_scheme().
proxy_scheme('true') -> 'https';
proxy_scheme('false') -> 'http'.

-spec proxy_default_port(proxy_scheme()) -> 80 | 443.
proxy_default_port('http') -> 80;
proxy_default_port('https') -> 443.

-spec proxy_creds(atom()) -> binary().
proxy_creds('proxy_playback') -> proxy_creds(?AUTH_PLAYBACK);
proxy_creds('proxy_store') -> proxy_creds(?USE_AUTH_STORE);
proxy_creds('true') -> list_to_binary([?AUTH_USERNAME, ":", ?AUTH_PASSWORD, "@"]);
proxy_creds('false') -> <<>>.

-spec proxy_base_url() -> kz_term:ne_binary().
proxy_base_url() ->
    proxy_base_url('proxy_playback').

-spec proxy_base_url(kz_term:ne_binary() | atom()) -> kz_term:ne_binary().
proxy_base_url(<<"store">>) ->
    proxy_base_url('proxy_store');
proxy_base_url(Type)
  when is_binary(Type) ->
    proxy_base_url('proxy_playback');
proxy_base_url(Type) ->
    case kapps_config:get_json(?CONFIG_CAT, <<"distributed_proxy">>) of
        'undefined' -> base_url(proxy_host(), proxy_port(), Type);
        JObj -> maybe_distributed_proxy_url(Type, JObj)
    end.

-spec maybe_distributed_proxy_url(atom(), kz_json:object()) -> kz_term:ne_binary().
maybe_distributed_proxy_url(Type, JObj) ->
    case kz_json:is_true(<<"enabled">>, JObj, 'false') of
        'false' -> base_url(proxy_host(), proxy_port(), Type);
        'true' -> distributed_proxy_url(Type, JObj)
    end.

-spec distributed_proxy_url(atom(), kz_json:object()) -> kz_term:ne_binary().
distributed_proxy_url(Type, JObj) ->
    Host = kz_json:get_ne_binary_value(<<"hostname">>, JObj, proxy_host()),
    Scheme = kz_json:get_atom_value(<<"scheme">>, JObj, 'http'),
    Port = kz_json:get_integer_value(<<"port">>, JObj, proxy_default_port(Scheme)),
    Creds = proxy_creds(Type),
    build_url(Scheme, Host, Port, Creds).
