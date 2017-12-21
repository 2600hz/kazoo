%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Azure for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------

-module(kz_att_azure).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

%% ====================================================================
%% `gen_attachment' behaviour callbacks (API)
%% ====================================================================
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(Settings, DbName, DocId, AName, Contents, _Options) ->
    CT = kz_mime:from_filename(AName),

    {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
    Pid = azure_pid(Settings),
    AzureOptions = [{content_type, kz_term:to_list(CT)}, return_headers],
    try
        case erlazure:put_block_blob(Pid, Container, Name, Contents, AzureOptions) of
            {ok, Headers, _Body} ->
                props:to_log(Headers, <<"AZURE HEADERS">>),
                Data = base64:encode(term_to_binary({Settings#{file => Name}, Name})),
                Metadata = kz_json:from_list(kz_att_util:headers_as_binaries(Headers)),
                {'ok', [{'attachment', [{<<"azure">>, Data}
                                       ,{<<"metadata">>, Metadata}
                                       ]}
                       ]};
            {'error', ErrorCode} = _E when is_atom(ErrorCode) -> % from erlazure:execute_request/2
                lager:error("error storing attachment in azure : ~p", [_E]),
                gen_attachment:error_response(ErrorCode, <<"Error storing attachment in Azure">>);
            {'error', ErrorBody} = _E -> % from erlazure:execute_request/2
                lager:error("error storing attachment in azure : ~p", [_E]),
                gen_attachment:error_response(400, ErrorBody);
            SomethingElse ->
                lager:error("Failed to put the attachment to Azure: ~p", [SomethingElse]),
                gen_attachment:error_response(400, <<"Something failed(?)">>)
        end
    catch
        _:{{{'case_clause', {'error', {'failed_connect', _}}}, _StackTrace}, _FnFailing} ->
            lager:debug("Failed to put the attachment.~nStackTrace: ~p~nFnFailing: ~p",
                        [_StackTrace, _FnFailing]),
            gen_attachment:error_response(400, <<"Failed to connect. Check your settings">>)
    end.


-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    case kz_json:get_value(<<"azure">>, HandlerProps) of
        'undefined' ->
            gen_attachment:error_response(400, 'invalid_data');
        AzureData ->
            {Settings, _ContentId} = binary_to_term(base64:decode(AzureData)),
            Pid = azure_pid(Settings),
            {Container, Name} = resolve_path(Settings, {DbName, DocId, AName}),
            case erlazure:get_blob(Pid, Container, Name) of
                {'error', Reason} -> gen_attachment:error_response(400, Reason);
                {'ok', _RespBody} = Resp -> Resp
            end
    end.

%% ====================================================================
%% Internal functions
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
