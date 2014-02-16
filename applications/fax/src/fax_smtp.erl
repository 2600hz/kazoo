%% @author root
%% @doc @todo Add description to fax_smtp.


-module(fax_smtp).
-behaviour(gen_smtp_server_session).

-include("fax.hrl").

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
	handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
	handle_other/3, handle_AUTH/4, handle_STARTTLS/1, code_change/3, terminate/2]).

-define(RELAY, true).

-record(state,
	{
		options = [] :: list(),
		from :: binary(),
		docs = [] :: list(),
		filename :: binary(),
		content_type :: binary(),
		peer_ip :: tuple(),
		proxy_ip :: tuple()
	}).

-type(error_message() :: {'error', string(), #state{}}).

-spec init(Hostname :: binary(), SessionCount :: non_neg_integer(), Address :: tuple(), Options :: list()) -> {'ok', string(), #state{}} | {'stop', any(), string()}.
init(Hostname, SessionCount, Address, Options) ->
	%lager:info("peer: ~p", [Address]),
	case SessionCount > 20 of
		false ->
			Banner = [Hostname, " Kazoo Email to Fax Server"],
			State = #state{options = Options, peer_ip = Address},
			{ok, Banner, State};
		true ->
			lager:info("Connection limit exceeded"),
			{stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}
	end.

-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'ok', pos_integer(), #state{}} | {'ok', #state{}} | error_message().
handle_HELO(<<"invalid">>, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
	{ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
	lager:info("HELO from ~s", [Hostname]),
	{ok, 655360, State}. % 640kb of HELO should be enough for anyone.
	%If {ok, State} was returned here, we'd use the default 10mb limit

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'ok', list(), #state{}} | error_message().
handle_EHLO(<<"invalid">>, _Extensions, State) ->
	% contrived example
	{error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
	lager:info("EHLO from ~s", [Hostname]),
	% You can advertise additional extensions, or remove some defaults
	MyExtensions = case proplists:get_value(auth, State#state.options, false) of
		true ->
			% auth is enabled, so advertise it
			Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
		false ->
			Extensions
	end,
	{ok, MyExtensions, State}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | error_message().
handle_MAIL(From, State) ->
	lager:info("Checking Mail from ~s", [From]),
	{ok, State#state{from=From}}.

-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(Extension, _State) ->
	lager:info("Unknown MAIL FROM extension ~s", [Extension]),
	error.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(To, State) ->
	lager:info("Checking Mail to ~s", [To]),
	check_faxbox(To,State).


-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(Extension, _State) ->
	lager:info("Unknown RCPT TO extension ~s", [Extension]),
	error.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, To, <<>>, State) ->
	lager:info("552 Message too small. From ~p to ~p", [From,To]),
	{error, "552 Message too small", State};
handle_DATA(From, To, Data, State) ->
	lager:info("Handle Data From ~p to ~p", [From,To]),
	Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),
	Result = try mimemail:decode(Data) of
		{Type,SubType,Headers,Parameters,Body} ->
			lager:info("Message decoded successfully!~n"),
			{ProcessResult, NewState} = process_message(Type,SubType,Headers,Parameters,Body,State),
			{ProcessResult, Reference, NewState};
		Other -> lager:info("mime decode other ~p",[Other]),
	 			{error, <<"Message decode failed">>, State}
	catch
		What:Why ->
			lager:info("Message decode FAILED with ~p:~p", [What, Why]),
			case proplists:get_value(dump, State#state.options, false) of
			false -> ok;
			true ->
				%% optionally dump the failed email somewhere for analysis
				File = "/tmp/"++Reference,
				case filelib:ensure_dir(File) of
					ok -> file:write_file(File, Data);
					_ -> ok
				end
			end,
			{error, <<"Message decode failed">>, State}
	end,
	% At this point, if we return ok, we've accepted responsibility for the email
	Result.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
    lager:info("RSET Called"),
	% reset any relevant internal state
	State.

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
    lager:info("252 VRFY disabled by policy, just send some mail"),
	{error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(<<"PROXY">>, Args, State) ->
    lager:info("PROXY : ~p",[Args]),
	{State};
%% TODO
%% <<"PROXY">> / <<"TCP4 X.X.X.X Y.Y.Y.Y 59640 25">>
%% TRANSPORT PEER_IP PROXY_IP PEER_PORT PROXY_PORT

handle_other(Verb, Args, State) ->
	% You can implement other SMTP verbs here, if you need to
    lager:info("500 Error: command not recognized : ~p / ~p",[Verb,Args]),
	{["500 Error: command not recognized : '", Verb, "'"], State}.


-spec handle_AUTH(Type :: 'login' | 'plain' | 'cram-md5', Username :: binary(), Password :: binary() | {binary(), binary()}, #state{}) -> {'ok', #state{}} | 'error'.
handle_AUTH(_Type, _Username, _Password, _State) ->
	error.

-spec handle_STARTTLS(#state{}) -> #state{}.
handle_STARTTLS(State) ->
    lager:info("SMTP TLS Started"),
    State.

-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(normal, #state{filename=Filename,content_type=CT,docs=Docs}=State) ->
	case wh_util:is_empty(Filename) of
		'false' ->
            lager:info("terminate normal saving docs"),
			{ok,FileContents} = file:read_file(Filename),
			save_fax_doc(Docs, FileContents,CT),
			file:delete(Filename);
		_  -> ok
	end,
	{ok, normal, State};
terminate(Reason, State) ->
	lager:info("Terminate ~p", [Reason]),
	{ok, Reason, State}.




%%% Internal Functions %%%


save_fax_doc([],_FileContents, _CT) ->
	ok;
save_fax_doc([Doc|Docs],FileContents,CT) ->
	case couch_mgr:save_doc(?WH_FAXES, Doc) of
		{'ok', JObj} -> DocId = wh_json:get_value(<<"_id">>, JObj),
						Rev = wh_json:get_value(<<"_rev">>, JObj),
						Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]},{'rev', Rev}],
						couch_mgr:put_attachment(?WH_FAXES, DocId, attachment_name(<<>>, CT), FileContents, Opts);
		Else -> ok
	end,
	save_fax_doc(Docs,FileContents,CT).


-spec check_faxbox(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
check_faxbox(To, #state{from=From}=State) ->
	[FaxNumber,FaxBoxId] = binary:split(wh_util:to_lower_binary(To),<<"@">>),
	ViewOptions = [{<<"key">>, FaxBoxId},'include_docs'],
    case couch_mgr:get_results(?WH_FAXES, <<"faxbox/email_address">>, ViewOptions) of
        {'ok', []} -> {error, <<"Not Found">>, State};
        {'ok', [JObj]} -> check_faxbox_permissions(FaxNumber, wh_json:get_value(<<"doc">>,JObj), State );
        {'error', 'not_found'} -> {error, <<"Not Found">>, State};
        _ -> {error, <<"Unknown Error">>, State}
    end.

-spec check_faxbox_permissions(FaxNumber :: binary(), FaxBoxDoc :: wh_json:object() , State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
check_faxbox_permissions(FaxNumber, FaxBoxDoc, #state{from=From}=State) ->
	lager:info("Checking if ~s can send to ~p. doc is ~p",[From,wh_json:get_value(<<"name">>,FaxBoxDoc),FaxBoxDoc]),
	add_fax_document(FaxNumber, FaxBoxDoc, State).
%	{ok, State}.

add_fax_document(FaxNumber, FaxBoxDoc, #state{docs=Docs}=State) ->
	FaxBoxId = wh_json:get_value(<<"_id">>,FaxBoxDoc),
	AccountId = wh_json:get_value(<<"pvt_account_id">>,FaxBoxDoc),
	AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
	Props = [
			 {<<"from_name">>,wh_json:get_value(<<"caller_name">>,FaxBoxDoc)}
            ,{<<"fax_identity_name">>, wh_json:get_value(<<"caller_name">>, FaxBoxDoc)}		
			,{<<"from_number">>,wh_json:get_value(<<"caller_id">>,FaxBoxDoc)}
            ,{<<"fax_identity_number">>, wh_json:get_value(<<"caller_id">>, FaxBoxDoc)}
            ,{<<"fax_timezone">>, wh_json:get_value(<<"timezone">>, FaxBoxDoc)}
			,{<<"to_name">>,FaxNumber}
			,{<<"to_number">>,FaxNumber}
			,{<<"retries">>,wh_json:get_value(<<"retries">>,FaxBoxDoc,3)}
			,{<<"notifications">>,wh_json:from_list([{<<"email">>,wh_json:from_list([{<<"send_to">>,wh_json:get_value(<<"email_to">>,FaxBoxDoc)}])}]) }
            ,{<<"faxbox_id">>, FaxBoxId}
            ,{<<"folder">>, <<"outbox">>}
			 ],
	{ _ , JObj} = wh_json_validator:is_valid(wh_json:from_list(Props), <<"faxes">>),
	Doc = wh_json:set_values([{<<"pvt_type">>, <<"fax">>}
                             ,{<<"pvt_job_status">>, <<"pending">>}
                             ,{<<"attempts">>, 0}
                             ,{<<"pvt_account_id">>, AccountId}
                             ,{<<"pvt_account_db">>, AccountDb}], JObj),
	NewDocs = [Doc | Docs],
	{ok, State#state{docs=NewDocs}}.




%% ====================================================================
%% Internal functions
%% ====================================================================

process_message(<<"multipart">>,<<"mixed">>,_Headers,_Parameters,Body, State) ->
	lager:info("processing multipart/mixed"),
	case Body of
		{Type,SubType,Headers,Parameters,BodyPart} ->
			lager:info("processing ~s/~s",[Type,SubType]),
			process_part(<<Type/binary,"/",SubType/binary>>,Headers,Parameters,BodyPart, State);
		[{Type,SubType,Headers,Parameters,BodyParts}|OtherParts]=Parts ->
			lager:info("processing multiple parts, first is ~s/~s",[Type,SubType]),
			process_parts(Parts, State);
		A -> lager:info("missed processing ~p",[A]),
			 {ok,State}
	end;
process_message(Type,SubType,Headers,Parameters,Body, State) ->
	lager:info("skipping ~s/~s",[Type,SubType]),
	{ok,State}.

process_parts([],State) ->
	{ok, State};
process_parts([Part|Parts], State) ->
	case Part of
		{Type,SubType,Headers,Parameters,BodyPart} ->
			{_ , NewState} = process_part(<<Type/binary,"/",SubType/binary>>,Headers,Parameters,BodyPart,State),
			process_parts(Parts,NewState);
		A -> lager:info("missed parts processing ~p",[A]),
			 {ok , State}
	end.
					
	
process_part(<<"application/pdf">>=CT,Headers,Parameters,Body,State) ->
    lager:info("Part is application/pdf"),
	FileName = <<"/tmp/email_attachment_",(wh_util:to_binary(wh_util:current_tstamp()))/binary,".pdf">>,
	file:write_file(FileName,Body),
    {ok, State#state{filename=FileName,content_type=CT}};
process_part(<<"image/tiff">>=CT,Headers,Parameters,Body,State) ->
    lager:info("Part is image/tiff"),
	FileName = <<"/tmp/email_attachment_",(wh_util:to_binary(wh_util:current_tstamp()))/binary,".tif">>,
	file:write_file(FileName,Body),
    {ok, State#state{filename=FileName,content_type=CT}};
process_part(ContentType,Headers,Parameters,Body,State) ->
	lager:info("Ignoring Part ~s",[ContentType]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              true -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              false -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               false -> A;
                               true ->
                                   <<A/binary, ".", (content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    F = lists:foldl(fun(F, A) -> F(A) end, Filename, Generators),
	lager:info("attachment_name ~p",[F]),
	F.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known media types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension(ne_binary()) -> ne_binary().
content_type_to_extension(<<"application/pdf">>) -> <<"pdf">>;
content_type_to_extension(<<"image/tiff">>) -> <<"tiff">>.
