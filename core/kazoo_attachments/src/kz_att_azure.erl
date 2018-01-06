%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Azure for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_azure).

-include("kz_att.hrl").

-export([put_attachment/6]).
-export([fetch_attachment/4]).


%% ====================================================================
%% API functions
%% ====================================================================


-spec resolve_path(map(), attachment_info()) -> {string(), string()}.
resolve_path(#{container := Container} = Settings, AttInfo) ->
    Url = azure_format_url(Settings, AttInfo),
    {kz_term:to_list(Container), kz_term:to_list(Url)}.

-spec azure_default_fields() -> kz_term:proplist().
azure_default_fields() ->
    [{group, [{arg, <<"id">>}
             ,<<"_">>
             ,{arg, <<"attachment">>}
             ]}
    ].

-spec azure_format_url(map(), attachment_info()) -> kz_term:ne_binary().
azure_format_url(Map, AttInfo) ->
    kz_att_util:format_url(Map, AttInfo, azure_default_fields()).

azure_pid(#{account := Account, key := Key}) ->
    azure_pid(Account, Key).

azure_pid(Account, Key) ->
    case kz_att_azure_sup:worker(Account) of
        undefined ->
            case kz_att_azure_sup:start_azure(Account, Key) of
                {ok, Pid} -> Pid;
                {error,{already_started,Pid}} -> Pid
            end;
        Pid -> Pid
    end.

-spec put_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> any().
put_attachment(Settings, DbName, DocId, AName, Contents, _Options) ->
    CT = kz_mime:from_filename(AName),

    {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
    Pid = azure_pid(Settings),
    AzureOptions = [{content_type, kz_term:to_list(CT)}, return_headers],
    case erlazure:put_block_blob(Pid, Container, Name, Contents, AzureOptions) of
        {ok, Headers, _Body} ->
            props:to_log(Headers, <<"AZURE HEADERS">>),
            Data = base64:encode(term_to_binary({Settings#{file => Name}, Name})),
            Metadata = kz_json:from_list(kz_att_util:headers_as_binaries(Headers)),
            {'ok', [{'attachment', [{<<"azure">>, Data}
                                   ,{<<"metadata">>, Metadata}
                                   ]}
                   ]};
        {error, _E} = Error ->
            lager:error("error storing attachment in azure : ~p", [_E]),
            Error
    end.


-spec fetch_attachment(kz_data:connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                              {'ok', iodata()} |
                              {'error', 'invalid_data' | 'not_found'}.
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"azure">>, HandlerProps) of
        'undefined' -> {'error', 'invalid_data'};
        AzureData ->
            {Settings, _ContentId} = binary_to_term(base64:decode(AzureData)),
            Pid = azure_pid(Settings),
            {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
            erlazure:get_blob(Pid, Container, Name)
    end.
