%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------

-module(fax_smtp).
-behaviour(gen_smtp_server_session).

-export([init/4
         ,handle_HELO/2
         ,handle_EHLO/3
         ,handle_MAIL/2
         ,handle_MAIL_extension/2
         ,handle_RCPT/2
         ,handle_RCPT_extension/2
         ,handle_DATA/4
         ,handle_RSET/1
         ,handle_VRFY/2
         ,handle_other/3
         ,handle_AUTH/4
         ,handle_STARTTLS/1
         ,code_change/3
         ,terminate/2
        ]).

-include("fax.hrl").

-define(RELAY, 'true').

-record(state, {
          options = [] :: list()
          ,from :: binary()
          ,docs = [] :: list()
          ,filename :: binary()
          ,content_type :: binary()
          ,peer_ip :: tuple()
          ,proxy_ip :: tuple()
         }).

-type error_message() :: {'error', string(), #state{}}.

-spec init(ne_binary(), non_neg_integer(), tuple(), list()) ->
                  {'ok', string(), #state{}} |
                  {'stop', any(), string()}.
init(Hostname, SessionCount, Address, Options) ->
    case SessionCount > 20 of
        'false' ->
            Banner = [Hostname, " Kazoo Email to Fax Server"],
            State = #state{options = Options, peer_ip = Address},
            {'ok', Banner, State};
        'true' ->
            lager:info("connection limit exceeded ~p", [Address]),
            {'stop', 'normal', ["421 ", Hostname, " is too busy to accept mail right now"]}
    end.

-spec handle_HELO(binary(), #state{}) ->
                         {'ok', pos_integer(), #state{}} |
                         {'ok', #state{}} |
                         error_message().
handle_HELO(<<"invalid">>, State) ->
    %% contrived example
    {'error', "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {'ok', State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    lager:info("HELO from ~s", [Hostname]),
    {'ok', 655360, State}. % 640kb of HELO should be enough for anyone.
%% If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(binary(), list(), #state{}) ->
                         {'ok', list(), #state{}} |
                         error_message().
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    %% contrived example
    {'error', "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, #state{options=Options}=State) ->
    lager:info("EHLO from ~s", [Hostname]),
    %% You can advertise additional extensions, or remove some defaults
    MyExtensions = case props:get_is_true('auth', Options, 'false') of
                       'false' -> Extensions;
                       'true' ->
                           %% auth is enabled, so advertise it
                           [{"AUTH", "PLAIN LOGIN CRAM-MD5"}
                            ,{"STARTTLS", 'true'}
                            | Extensions
                           ]
                   end,
    {'ok', MyExtensions, State}.

-spec handle_MAIL(binary(), #state{}) ->
                         {'ok', #state{}} |
                         error_message().
handle_MAIL(From, State) ->
    lager:info("Checking Mail from ~s", [From]),
    {'ok', State#state{from=From}}.

-spec handle_MAIL_extension(binary(), #state{}) ->
                                   {'ok', #state{}} |
                                   'error'.
handle_MAIL_extension(Extension, _State) ->
    lager:info("Unknown MAIL FROM extension ~s", [Extension]),
    'error'.

-spec handle_RCPT(binary(), #state{}) ->
                         {'ok', #state{}} |
                         {'error', string(), #state{}}.
handle_RCPT(To, State) ->
    lager:info("Checking Mail to ~s", [To]),
    check_faxbox(To,State).

-spec handle_RCPT_extension(binary(), #state{}) ->
                                   {'ok', #state{}} |
                                   'error'.
handle_RCPT_extension(Extension, _State) ->
    lager:info("Unknown RCPT TO extension ~s", [Extension]),
    'error'.

-spec handle_DATA(binary(), ne_binaries(), binary(), #state{}) ->
                         {'ok', string(), #state{}} |
                         {'error', string(), #state{}}.
handle_DATA(From, To, <<>>, State) ->
    lager:info("552 Message too small. From ~p to ~p", [From,To]),
    {'error', "552 Message too small", State};
handle_DATA(From, To, Data, #state{options=Options}=State) ->
    lager:info("Handle Data From ~p to ~p", [From,To]),

    %% JMA: Can this be done with wh_util:rand_hex_binary() ?
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),

    try mimemail:decode(Data) of
        {Type,SubType,Headers,Parameters,Body} ->
            lager:info("Message decoded successfully!~n"),
            {ProcessResult, NewState} = process_message(Type,SubType,Headers,Parameters,Body,State),
            {ProcessResult, Reference, NewState};
        Other ->
            lager:info("mime decode other ~p",[Other]),
            {'error', <<"Message decode failed">>, State}
    catch
        _What:_Why ->
            lager:info("Message decode FAILED with ~p:~p", [_What, _Why]),
            case props:get_is_true('dump', Options, 'false') of
                'false' -> 'ok';
                'true' ->
                    %% optionally dump the failed email somewhere for analysis
                    File = "/tmp/"++Reference,
                    case filelib:ensure_dir(File) of
                        'ok' -> file:write_file(File, Data);
                        _ -> 'ok'
                    end
            end,
            {'error', <<"Message decode failed">>, State}
    end.

-spec handle_RSET(#state{}) -> #state{}.
handle_RSET(State) ->
    lager:info("RSET Called"),
    %% reset any relevant internal state
    State.

-spec handle_VRFY(binary(), #state{}) ->
                         {'ok', string(), #state{}} |
                         {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    lager:info("252 VRFY disabled by policy, just send some mail"),
    {'error', "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(binary(), binary(), #state{}) ->
                          {string(), #state{}} | {#state{}}.
handle_other(<<"PROXY">>, Args, State) ->
    lager:info("PROXY : ~p",[Args]),
    {State};

%% TODO
%% <<"PROXY">> / <<"TCP4 X.X.X.X Y.Y.Y.Y 59640 25">>
%% TRANSPORT PEER_IP PROXY_IP PEER_PORT PROXY_PORT

handle_other(Verb, Args, State) ->
    %% You can implement other SMTP verbs here, if you need to
    lager:info("500 Error: command not recognized : ~p / ~p",[Verb,Args]),
    {["500 Error: command not recognized : '", Verb, "'"], State}.

-spec handle_AUTH('login' | 'plain' | 'cram-md5', binary(), binary() | {binary(), binary()}, #state{}) ->
                         {'ok', #state{}} |
                         'error'.
handle_AUTH(_Type, _Username, _Password, _State) ->
    'error'.

-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    lager:info("SMTP TLS Started"),
    State.

-spec code_change(any(), #state{}, any()) -> {'ok', #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec terminate(any(), #state{}) ->
                       {'ok', any(), #state{}}.
terminate('normal', #state{filename=Filename
                           ,content_type=CT
                           ,docs=Docs
                          }=State) ->
    case wh_util:is_empty(Filename) of
        'false' ->
            lager:info("terminate normal saving docs"),
            {'ok', FileContents} = file:read_file(Filename),
            save_fax_doc(Docs, FileContents,CT),
            file:delete(Filename);
        _  -> 'ok'
    end,
    {'ok', 'normal', State};
terminate(Reason, State) ->
    lager:info("terminate ~p", [Reason]),
    {'ok', Reason, State}.

%%% Internal Functions %%%
save_fax_doc([],_FileContents, _CT) -> 'ok';
save_fax_doc([Doc|Docs], FileContents, CT) ->
    case couch_mgr:save_doc(?WH_FAXES, Doc) of
        {'ok', JObj} ->
            DocId = wh_json:get_value(<<"_id">>, JObj),
            Rev = wh_json:get_value(<<"_rev">>, JObj),
            Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}
                    ,{'rev', Rev}
                   ],
            Name = attachment_name(<<>>, CT),
            lager:debug("attachment name: ~s", [Name]),
            couch_mgr:put_attachment(?WH_FAXES, DocId, Name, FileContents, Opts);
        _Else -> 'ok'
    end,
    save_fax_doc(Docs,FileContents,CT).

-spec check_faxbox(binary(), #state{}) ->
                          {'ok', #state{}} |
                          {'error', string(), #state{}}.
check_faxbox(To, State) ->
    [FaxNumber,FaxBoxId] = binary:split(wh_util:to_lower_binary(To),<<"@">>),
    ViewOptions = [{'key', FaxBoxId}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_FAXES, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> {'error', <<"Not Found">>, State};
        {'ok', [JObj]} -> check_faxbox_permissions(FaxNumber, wh_json:get_value(<<"doc">>,JObj), State );
        {'error', 'not_found'} -> {'error', <<"Not Found">>, State};
        _ -> {'error', <<"Unknown Error">>, State}
    end.

-spec check_faxbox_permissions(binary(), wh_json:object(), #state{}) ->
                                      {'ok', #state{}} |
                                      {'error', string(), #state{}}.
check_faxbox_permissions(FaxNumber, FaxBoxDoc, #state{from=From}=State) ->
    lager:info("checking if ~s can send to ~p. doc is ~p",[From,wh_json:get_value(<<"name">>,FaxBoxDoc),FaxBoxDoc]),
    add_fax_document(FaxNumber, FaxBoxDoc, State).

add_fax_document(FaxNumber, FaxBoxDoc, #state{docs=Docs}=State) ->
    FaxBoxId = wh_json:get_value(<<"_id">>,FaxBoxDoc),
    AccountId = wh_json:get_value(<<"pvt_account_id">>,FaxBoxDoc),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Props = props:filter_undefined(
              [{<<"from_name">>,wh_json:get_value(<<"caller_name">>,FaxBoxDoc)}
               ,{<<"fax_identity_name">>, wh_json:get_value(<<"fax_header">>, FaxBoxDoc)}
               ,{<<"from_number">>,wh_json:get_value(<<"caller_id">>,FaxBoxDoc)}
               ,{<<"fax_identity_number">>, wh_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
               ,{<<"fax_timezone">>, wh_json:get_value(<<"fax_timezone">>, FaxBoxDoc)}
               ,{<<"to_name">>,FaxNumber}
               ,{<<"to_number">>,FaxNumber}
               ,{<<"retries">>,wh_json:get_value(<<"retries">>,FaxBoxDoc,3)}
               ,{<<"notifications">>
                 ,wh_json:from_list([{<<"email">>
                                      ,wh_json:from_list([{<<"send_to">>
                                                           ,wh_json:get_value(<<"email_to">>, FaxBoxDoc)
                                                          }
                                                         ])
                                     }
                                    ])}
               ,{<<"faxbox_id">>, FaxBoxId}
               ,{<<"folder">>, <<"outbox">>}
              ]),
    { _ , JObj} = wh_json_validator:is_valid(wh_json:from_list(Props), <<"faxes">>),
    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                              ,{<<"pvt_job_status">>, <<"pending">>}
                              ,{<<"pvt_created">>, wh_util:current_tstamp()}      
                              ,{<<"attempts">>, 0}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                             ], JObj),
    {'ok', State#state{docs=[Doc | Docs]}}.




%% ====================================================================
%% Internal functions
%% ====================================================================
process_message(<<"multipart">>, <<"mixed">>, _Headers, _Parameters, Body, State) ->
    lager:info("processing multipart/mixed"),
    case Body of
        {Type, SubType, HeadersPart, ParametersPart, BodyPart} ->
            lager:info("processing ~s/~s",[Type,SubType]),
            process_part(<<Type/binary,"/",SubType/binary>>,HeadersPart,ParametersPart,BodyPart, State);
        [{Type, SubType, _HeadersPart, _ParametersPart, _BodyParts}|_OtherParts]=Parts ->
            lager:info("processing multiple parts, first is ~s/~s",[Type,SubType]),
            process_parts(Parts, State);
        A ->
            lager:info("missed processing ~p",[A]),
            {'ok', State}
    end;
process_message(_Type, _SubType, _Headers, _Parameters, _Body, State) ->
    lager:info("skipping ~s/~s",[_Type, _SubType]),
    {'ok', State}.

process_parts([], State) ->
    {'ok', State};
process_parts([Part|Parts], State) ->
    case Part of
        {Type,SubType,Headers,Parameters,BodyPart} ->
            {_ , NewState} = process_part(<<Type/binary,"/",SubType/binary>>,Headers,Parameters,BodyPart,State),
            process_parts(Parts,NewState);
        A ->
            lager:info("missed parts processing ~p",[A]),
            {'ok', State}
    end.

process_part(<<"application/pdf">>=CT, _Headers, _Parameters, Body, State) ->
    lager:info("part is application/pdf"),
    Filename = <<"/tmp/email_attachment_",(wh_util:to_binary(wh_util:current_tstamp()))/binary,".pdf">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(<<"image/tiff">>=CT, _Headers, _Parameters, Body, State) ->
    lager:info("Part is image/tiff"),
    Filename = <<"/tmp/email_attachment_",(wh_util:to_binary(wh_util:current_tstamp()))/binary,".tiff">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(_ContentType, _Headers, _Parameters, _Body, State) ->
    lager:debug("ignoring Part ~s",[_ContentType]),
    {'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun maybe_generate_random_filename/1
                  ,fun(A) -> maybe_attach_extension(A, CT) end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

maybe_generate_random_filename(A) ->
    case wh_util:is_empty(A) of
        'true' -> wh_util:to_hex_binary(crypto:rand_bytes(16));
        'false' -> A
    end.

-spec maybe_attach_extension(ne_binary(), ne_binary()) -> ne_binary().
maybe_attach_extension(A, CT) ->
    case wh_util:is_empty(filename:extension(A)) of
        'false' -> A;
        'true' -> <<A/binary, ".", (content_type_to_extension(CT))/binary>>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(ne_binary()) -> ne_binary().
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"image/tiff">>) -> <<"tiff">>.
