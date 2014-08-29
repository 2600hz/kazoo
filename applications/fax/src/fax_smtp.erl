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
-define(SMTP_MAX_SESSIONS, whapps_config:get_integer(?CONFIG_CAT, <<"smtp_sessions">>, 50)).

-record(state, {
          options = [] :: list()
          ,from :: binary()
          ,docs = [] :: list()
          ,filename :: binary()
          ,content_type :: binary()
          ,peer_ip :: tuple()
          ,proxy_ip :: tuple()
         }).

-type state() :: #state{}.
-type error_message() :: {'error', string(), #state{}}.

-spec init(ne_binary(), non_neg_integer(), tuple(), list()) ->
                  {'ok', string(), #state{}} |
                  {'stop', any(), string()}.
init(Hostname, SessionCount, Address, Options) ->
    case SessionCount > ?SMTP_MAX_SESSIONS  of
        'false' ->
            Banner = [Hostname, " Kazoo Email to Fax Server"],
            State = #state{options = Options, peer_ip = Address},
            {'ok', Banner, State};
        'true' ->
            lager:debug("connection limit exceeded ~p", [Address]),
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
    lager:debug("HELO from ~s", [Hostname]),
    {'ok', 655360, State}. % 640kb of HELO should be enough for anyone.
%% If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(binary(), list(), #state{}) ->
                         {'ok', list(), #state{}} |
                         error_message().
handle_EHLO(<<"invalid">>, _Extensions, State) ->
    %% contrived example
    {'error', "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, #state{options=Options}=State) ->
    lager:debug("EHLO from ~s", [Hostname]),
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
    lager:debug("Checking Mail from ~s", [From]),
    {'ok', State#state{from=From}}.

-spec handle_MAIL_extension(binary(), #state{}) ->
                                   {'ok', #state{}} |
                                   'error'.
handle_MAIL_extension(Extension, _State) ->
    lager:debug("Unknown MAIL FROM extension ~s", [Extension]),
    'error'.

-spec handle_RCPT(binary(), #state{}) ->
                         {'ok', #state{}} |
                         {'error', string(), #state{}}.
handle_RCPT(To, State) ->
    lager:debug("Checking Mail to ~s", [To]),
    check_faxbox(To,State).

-spec handle_RCPT_extension(binary(), #state{}) ->
                                   {'ok', #state{}} |
                                   'error'.
handle_RCPT_extension(Extension, _State) ->
    lager:debug("Unknown RCPT TO extension ~s", [Extension]),
    'error'.

-spec handle_DATA(binary(), ne_binaries(), binary(), #state{}) ->
                         {'ok', string(), #state{}} |
                         {'error', string(), #state{}}.
handle_DATA(From, To, <<>>, State) ->
    lager:debug("552 Message too small. From ~p to ~p", [From,To]),
    {'error', "552 Message too small", State};
handle_DATA(From, To, Data, #state{options=Options}=State) ->
    lager:debug("Handle Data From ~p to ~p", [From,To]),

    %% JMA: Can this be done with wh_util:rand_hex_binary() ?
    Reference = lists:flatten(
                  [io_lib:format("~2.16.0b", [X])
                   || <<X>> <= erlang:md5(term_to_binary(erlang:now()))
                  ]),

    try mimemail:decode(Data) of
        {Type, SubType, Headers, Parameters, Body} ->
            lager:debug("Message decoded successfully!"),
            {ProcessResult, NewState}
                = process_message(Type, SubType, Headers, Parameters, Body, State),
            {ProcessResult, Reference, NewState};
        Other ->
            lager:debug("mime decode other ~p", [Other]),
            {'error', <<"Message decode failed">>, State}
    catch
        _What:_Why ->
            lager:debug("Message decode FAILED with ~p:~p", [_What, _Why]),
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
    lager:debug("RSET Called"),
    %% reset any relevant internal state
    State.

-spec handle_VRFY(binary(), #state{}) ->
                         {'ok', string(), #state{}} |
                         {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    lager:debug("252 VRFY disabled by policy, just send some mail"),
    {'error', "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(binary(), binary(), #state{}) ->
                          {string(), #state{}} | {#state{}}.
handle_other(<<"PROXY">>, Args, State) ->
    lager:debug("PROXY : ~p",[Args]),
    {State};


handle_other(Verb, Args, State) ->
    %% You can implement other SMTP verbs here, if you need to
    lager:debug("500 Error: command not recognized : ~p / ~p",[Verb,Args]),
    {["500 Error: command not recognized : '", Verb, "'"], State}.

-spec handle_AUTH('login' | 'plain' | 'cram-md5', binary(), binary() | {binary(), binary()}, #state{}) ->
                         {'ok', #state{}} |
                         'error'.
handle_AUTH(_Type, _Username, _Password, _State) ->
    'error'.

-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    lager:debug("SMTP TLS Started"),
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
            lager:debug("terminate normal saving docs"),
            {'ok', FileContents} = file:read_file(Filename),
            fax_util:save_fax_docs(Docs, FileContents,CT),
            file:delete(Filename);
        _  -> 'ok'
    end,
    {'ok', 'normal', State};
terminate(Reason, State) ->
    lager:debug("terminate ~p", [Reason]),
    {'ok', Reason, State}.

%%% Internal Functions %%%



-spec check_faxbox(binary(), #state{}) ->
                          {'ok', #state{}} |
                          {'error', string(), #state{}}.
check_faxbox(To, State) ->
    [FaxNumber, Domain] = binary:split(wh_util:to_lower_binary(To), <<"@">>),
    ViewOptions = [{'key', Domain}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_FAXES, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> {'error', "Not Found", State};
        {'ok', [JObj]} -> check_faxbox_permissions(FaxNumber, wh_json:get_value(<<"doc">>,JObj), State);
        {'error', 'not_found'} -> {'error', "Not Found", State};
        _ -> {'error', "Unknown Error", State}
    end.


-spec match(binary(), binary()) -> boolean().
match(Address, Element) ->
    case re:run(Address, Element) of
        'nomatch' -> 'false';
        _Else -> 'true'
    end.

-spec check_faxbox_permissions(binary(), wh_json:object(), #state{}) ->
                                      {'ok', #state{}} |
                                      {'error', string(), #state{}}.
check_faxbox_permissions(FaxNumber, FaxBoxDoc, #state{from=From}=State) ->
    lager:debug("checking if ~s can send to ~p."
               ,[From,wh_json:get_value(<<"name">>, FaxBoxDoc)]),
    case wh_json:get_value(<<"smtp_permission_list">>, FaxBoxDoc, []) of
        [] ->
            case whapps_config:get_is_true(<<"fax">>, <<"allow_all_addresses_when_empty">>, 'false') of
                'true' -> add_fax_document(FaxNumber, FaxBoxDoc, State);
                'false' ->
                    lager:debug("faxbox permissions is empty and policy doesn't allow it"),
                    {'error', "not allowed", State}
            end;
        Permissions ->
            case lists:any(fun(A) -> match(From, A) end, Permissions) of
                'true' -> add_fax_document(FaxNumber, FaxBoxDoc, State);
                'false' ->
                    lager:debug("address ~is not allowed on faxbox ~s",[From, wh_json:get_value(<<"_id">>,FaxBoxDoc)]),
                    {'error', "not allowed", State}
            end
    end.

add_fax_document(FaxNumber, FaxBoxDoc, #state{docs=Docs
                                             ,from=From}=State) ->
    FaxBoxId = wh_json:get_value(<<"_id">>, FaxBoxDoc),
    AccountId = wh_json:get_value(<<"pvt_account_id">>, FaxBoxDoc),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    FaxBoxEmailNotify = wh_json:get_value([<<"notifications">>
                                          ,<<"outbound">>
                                          ,<<"email">>
                                          ,<<"send_to">>
                                          ], FaxBoxDoc, []),
    FaxBoxNotify = wh_json:set_value([<<"notifications">>
                                     ,<<"outbound">>
                                     ,<<"email">>
                                     ,<<"send_to">>
                                     ]
                                    ,lists:usort([From | FaxBoxEmailNotify])
                                    ,FaxBoxDoc),
    Notify = wh_json:get_value([<<"notifications">>, <<"outbound">>], FaxBoxNotify),

    Props = props:filter_undefined(
              [{<<"from_name">>, wh_json:get_value(<<"caller_name">>, FaxBoxDoc)}
               ,{<<"fax_identity_name">>, wh_json:get_value(<<"fax_header">>, FaxBoxDoc)}
               ,{<<"from_number">>, wh_json:get_value(<<"caller_id">>, FaxBoxDoc)}
               ,{<<"fax_identity_number">>, wh_json:get_value(<<"fax_identity">>, FaxBoxDoc)}
               ,{<<"fax_timezone">>, wh_json:get_value(<<"fax_timezone">>, FaxBoxDoc)}
               ,{<<"to_name">>, FaxNumber}
               ,{<<"to_number">>, FaxNumber}
               ,{<<"retries">>, wh_json:get_value(<<"retries">>, FaxBoxDoc, 3)}
               ,{<<"notifications">>, Notify }
               ,{<<"faxbox_id">>, FaxBoxId}
               ,{<<"folder">>, <<"outbox">>}
              ]),

    Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                              ,{<<"pvt_job_status">>, <<"attaching files">>}
                              ,{<<"pvt_created">>, wh_util:current_tstamp()}
                              ,{<<"attempts">>, 0}
                              ,{<<"pvt_account_id">>, AccountId}
                              ,{<<"pvt_account_db">>, AccountDb}
                             ]
                             ,wh_json_schema:add_defaults(wh_json:from_list(Props), <<"faxes">>)
                            ),
    {'ok', State#state{docs=[Doc | Docs]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
process_message(<<"multipart">>, <<"mixed">>, _Headers, _Parameters, Body, State) ->
    lager:debug("processing multipart/mixed"),
    case Body of
        {Type, SubType, HeadersPart, ParametersPart, BodyPart} ->
            lager:debug("processing ~s/~s", [Type, SubType]),
            process_part(<<Type/binary, "/", SubType/binary>>
                        ,HeadersPart
                        ,ParametersPart
                        ,BodyPart
                        ,State);
        [{Type, SubType, _HeadersPart, _ParametersPart, _BodyParts}|_OtherParts]=Parts ->
            lager:debug("processing multiple parts, first is ~s/~s", [Type, SubType]),
            process_parts(Parts, State);
        A ->
            lager:debug("missed processing ~p", [A]),
            {'ok', State}
    end;
process_message(_Type, _SubType, _Headers, _Parameters, _Body, State) ->
    lager:debug("skipping ~s/~s",[_Type, _SubType]),
    {'ok', State}.

process_parts([], State) ->
    {'ok', State};
process_parts([Part|Parts], State) ->
    case Part of
        {Type, SubType, Headers, Parameters, BodyPart} ->
            {_ , NewState}
                = process_part(<<Type/binary, "/", SubType/binary>>
                              ,Headers
                              ,Parameters
                              ,BodyPart
                              ,State),
            process_parts(Parts, NewState);
        A ->
            lager:debug("missed parts processing ~p", [A]),
            {'ok', State}
    end.

process_part(<<"application/pdf">>=CT, _Headers, _Parameters, Body, State) ->
    lager:debug("part is application/pdf"),
    Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".pdf">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(<<"image/tiff">>=CT, _Headers, _Parameters, Body, State) ->
    lager:debug("Part is image/tiff"),
    Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
    file:write_file(Filename, Body),
    {'ok', State#state{filename=Filename
                       ,content_type=CT
                      }};
process_part(<<"application/octet-stream">>, _Headers, Parameters, Body, State) ->
    lager:debug("part is application/octet-stream, try check attachemnt filename extension"),
    case props:get_value(<<"disposition">>, Parameters) of
        <<"attachment">> ->
            Props = props:get_value(<<"disposition-params">>, Parameters),
            maybe_process_part_attachment(Props, Body, State);
         _Else ->
            lager:debug("part is not attachment"),
            {'ok', State}
    end;
process_part(_ContentType, _Headers, _Parameters, _Body, State) ->
    lager:debug("ignoring Part ~s",[_ContentType]),
    {'ok', State}.

-spec maybe_process_part_attachment(wh_proplist(), iolist(), state()) -> {'ok', state()}.
maybe_process_part_attachment(Props, Body, State) ->
    case props:get_value(<<"filename">>, Props) of
        'undefined' ->
            lager:debug("attachment without filename"),
            {'ok', State};
        Filename -> process_part_attachment(Filename, Body, State)
    end.

-spec process_part_attachment(ne_binary(), iolist(), state()) -> {'ok', state()}.
process_part_attachment(Filename, Body, State) ->
    case filename:extension(wh_util:to_lower_binary(Filename)) of
        <<".pdf">> ->
            lager:debug("found pdf filename extension, set content-type to application/pdf"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".pdf">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                              ,content_type="application/pdf"
                              }};
        <<".tiff">> ->
            lager:debug("found tiff filename extension, set content-type to image/tiff"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                              ,content_type="image/tiff"
                              }};
        <<".tif">> ->
            lager:debug("found tif filename extension, set content-type to image/tiff"),
            Filename = <<"/tmp/email_attachment_", (wh_util:to_binary(wh_util:current_tstamp()))/binary, ".tiff">>,
            file:write_file(Filename, Body),
            {'ok', State#state{filename=Filename
                              ,content_type="image/tiff"
                              }};
        _Else ->
            lager:debug("not acceptable filename extension ~p", [_Else]),
            {'ok', State}
    end.
