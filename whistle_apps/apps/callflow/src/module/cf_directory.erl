%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% The basic flow of a directory call:
%%% 1) Prompt: Please enter the first few letters of the person's
%%%  a) First entry in sort order (first or last name)
%%% 2) Receive MIN_DTMF dtmf tones
%%%  a) If timeout occurs:
%%%   1) Prompt: You need to specify a minimum of
%%%    a) MIN_DTMF
%%%    b) Prompt: letters of the person's name
%%%   2) go back into main #2
%%% 3) After receiving MIN_DTMF, filter table
%%% 4) Go into a next_dtmf wait loop
%%%  a) if timeout, prompt with # of matches, option to hear matches or continue pressing keys
%%%  b) if continue, go into next_dtmf wait loop
%%%  c) else go to play_matches
%%% 5) play_matches: play hd(matches), options to hear more or connect or continue pressing keys
%%%
%%% If the flag "asr_enabled" is set, send the asr AMQP request, wait for the ASR response, and use
%%% that for finding matches. Its more an all-or-nothing situation.
%%%
%%% The asr_provider key has the following properties:
%%%   "p_endpoint": "user_or_did@asr-server.com" %% The endpoint to bridge to
%%%   "p_account_id":"you@xmpp-server.com" %% the client's account id for receiving the text back
%%%   "p_account_pass":"secret" %% optional, password for the client's account
%%%   "p_lang":"us-EN" %% the language code for the ASR provider, defaults to "us-EN"
%%%
%%% So, the process becomes:
%%% 1) Prompt "Please say the name of the person you'd like to be connected to"
%%% 2) Send ASR request with CallID, ControlQ, and a response Q
%%% 3) Wait for ASR response with text of what was said
%%% 4) Find matches and iterate through the list, or go back to 1.
%%% @end
%%% Created : 20 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_directory).

-include("../callflow.hrl").

-export([handle/2]).

-define(DIR_DOCS_VIEW, <<"directories/users_listing">>).

-define(FIELDS, [<<"last_name">>, <<"first_name">>]). %% what fields to convert/keep for searching
-define(DTMF_ACCEPT_MATCH, <<"1">>).
-define(DTMF_REJECT_MATCH, <<"9">>).

-define(DTMF_RESULT_CONNECT, <<"1">>).
-define(DTMF_RESULT_NEXT, <<"2">>).
-define(DTMF_RESULT_CONTINUE, <<"3">>).
-define(DTMF_RESULT_START, <<"4">>).

-define(DTMF_NO_RESULT_CONTINUE, <<"1">>).
-define(DTMF_NO_RESULT_START, <<"2">>).

-define(TIMEOUT_DTMF, 4000).
-define(TIMEOUT_ENDPOINT, ?DEFAULT_TIMEOUT).

-type lookup_entry() :: {ne_binary(), {wh_json:json_object(), wh_json:json_object()}}. % {ID, {DialpadJObj, JObj}}
-type lookup_table() :: [lookup_entry(),...] | [].

-record(dbn_state, {
         sort_by = 'last' :: 'first' | 'last' %% last_name field, or first_name field
         ,min_dtmf = 3 :: pos_integer() %% how many DTMF tones to collect before lookups start
         ,max_dtmf = 0 :: non_neg_integer() %% the most DTMF tones to collect; if max is reached, play choices. If max == 0, no limit
         ,confirm_match = 'false' :: boolean() %% if false, once a match is made, connect the caller; if true, prompt for confirmation to connect
         ,digits_collected = <<>> :: binary() %% the digits collected from the caller so far
         ,asr_endpoint = 'undefined' :: 'undefined' | binary() %% where to send a call for ASR
         ,asr_account_id = 'undefined' :: 'undefined' | binary() %% client's ID (for receiving text version back) (via XMPP, AMQP, or someother provider-specific way)
         ,asr_account_pass = 'undefined' :: 'undefined' | binary() %% password for the client's account
         ,asr_lang = <<"us-EN">> :: ne_binary() %% language of the speaker
         ,orig_lookup_table = [] :: lookup_table() %% the O.G.
         }).

-record(prompts, {
          enter_person = <<"system_media/dir-enter_person">> %% Please enter the first few letters of the person's
         ,firstname = <<"system_media/dir-first_name">> %% first name
         ,lastname = <<"system_media/dir-last_name">> %% last name
         ,at_extension = <<"system_media/dir-at_extension">> %% at extension
         ,specify_minimum = <<"system_media/dir-specify_minimum">> %% you need to specify a minimum of
         ,letters_of_name = <<"system_media/dir-letters_of_person_name">> %% letters of the person's name
         ,no_matching_results = <<"system_media/dir-no_matching_results">> %% There were no matching results
         ,please_try_again = <<"system_media/dir-please_try_again">> %% Please try again
         ,result_number = <<"system_media/dir-result_number">> %% Result number
         ,start_new_search = <<"system_media/dir-start_new_search">> %% To start a new search
         ,to_search_by = <<"system_media/dir-to_search_by">> %% To search by
         ,for_next = <<"system_media/dir-for_next">> %% for the previous entry
         ,no_more_results = <<"system_media/dir-no_more_results">> %% no more results
         ,result_match = <<"system_media/dir-result_match">> %% result matched your search
         ,too_many_results = <<"system_media/dir-too_many_result">> %% your search returned too many results
         ,to_select_entry = <<"system_media/dir-to_select_entry">> %% to select this entry
         ,confirm_menu = <<"system_media/dir-confirm_menu">> %% press 1 to connect. press 9 to start over.
         ,found = <<"system_media/dir-found">> %% found
         ,please_continue = <<"system_media/dir-please_continue">> %% please continue entering letters
         ,invalid_key = <<"system_media/dir-invalid_key">> %% invalid key pressed
         ,result_menu = <<"system_media/dir-result_menu">> %% press one to connect. press two for the next result. press three to continue searching. press four to start over.
         ,no_results_menu = <<"system_media/dir-no_results_menu">> %% press one to continue searching. press two to start over.
         ,asr_instructions = <<"system_media/dir-asr_instructions">> %% Please say the name of the party you would like to call
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DirID = wh_json:get_value(<<"id">>, Data),
    AccountDb = whapps_call:account_db(Call),
    {ok, DirJObj} = couch_mgr:open_doc(AccountDb, DirID),

    %% probably move this to the account doc instead of on each directory doc
    DbN0 = case wh_json:is_true(<<"asr_enabled">>, DirJObj, false) of
               true ->
                   ASR = wh_json:get_value(<<"asr_provider">>, DirJObj, wh_json:new()),
                   ?LOG("Setting ASR data for directory lookup"),
                   #dbn_state{asr_endpoint = wh_json:get_value(<<"p_endpoint">>, ASR)
                              ,asr_account_id = wh_json:get_value(<<"p_account_id">>, ASR)
                              ,asr_account_pass = wh_json:get_value(<<"p_account_pass">>, ASR)
                             };
               false -> #dbn_state{}
           end,

    MinDTMF = wh_json:get_integer_value(<<"min_dtmf">>, DirJObj, 3),
    SortBy = get_sort_by(wh_json:get_value(<<"sort_by">>, DirJObj, <<"last_name">>)),
    LookupTable = build_lookup_table(get_dir_docs(DirID, AccountDb)),

    ?LOG_START("dial by name: ~s", [wh_json:get_value(<<"name">>, DirJObj)]),
    ?LOG("sort by: ~s", [SortBy]),

    DbN = DbN0#dbn_state{sort_by = SortBy
                         ,min_dtmf = MinDTMF
                         ,max_dtmf = get_max_dtmf(wh_json:get_integer_value(<<"max_dtmf">>, DirJObj, 0), MinDTMF)
                         ,confirm_match = wh_json:is_true(<<"confirm_match">>, DirJObj, false)
                         ,orig_lookup_table = LookupTable
                        },
    start_search(Call, #prompts{}, DbN, LookupTable).

-spec start_search/4 :: (whapps_call:call(), #prompts{}, #dbn_state{}, lookup_table()) -> 'ok'.
start_search(Call, Prompts, #dbn_state{asr_endpoint=undefined, sort_by=SortBy}=DbN, LookupTable) ->
    {ok, DTMFs} = play_start_instructions(Call, Prompts, SortBy),
    collect_min_digits(Call, Prompts, DbN, LookupTable, DTMFs);
start_search(Call, Prompts, DbN, LookupTable) ->
    ?LOG("Playing ASR instructions"),
    _ = play_asr_instructions(Call, Prompts, DbN),

    ?LOG("Wait for asr response"),
    case asr_response() of
        {ok, Text} ->
            ?LOG("ASR responded with ~s", [Text]),
            analyze_text(Call, Prompts, DbN, LookupTable, Text);
        {error, Msg} ->
            ?LOG("Error with ASR, reverting to old school DBN: ~p", [Msg]),
            start_search(Call, Prompts, DbN#dbn_state{asr_endpoint=undefined}, LookupTable)
    end.

-spec analyze_text/5 :: (whapps_call:call(), #prompts{}, #dbn_state{}, lookup_table(), ne_binary()) -> 'ok'.
analyze_text(Call, Prompts, #dbn_state{confirm_match=ConfirmMatch}=DbN, LookupTable, Text) ->
    case analyze_dtmf(Text, LookupTable) of
        no_results ->
            ok = play_no_results(Call, Prompts),
            start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table);
        {one_result, JObj} ->
            case confirm_match(Call, Prompts, ConfirmMatch, JObj) of
                true ->
                    route_to_match(Call, JObj);
                false ->
                    start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
            end;
        {many_results, Matches} ->
            ok = play_has_matches(Call, Prompts, length(Matches)),
            case matches_menu(Call, Prompts, Matches, whapps_call:account_db(Call)) of
                {route, JObj} -> route_to_match(Call, JObj);
                continue -> collect_next_dtmf(Call, Prompts, DbN, Matches);
                start_over -> start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
            end
    end.

asr_response() ->
    ?LOG("Waiting for ASR response"),
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    ?LOG("channel was destroyed while waiting for bridge"),
                    {error, hangup};
                {<<"call_event">>, <<"CHANNEL_HANGUP">>} ->
                    ?LOG("channel was hungup while waiting for bridge"),
                    {error, hangup};
                {<<"error">>, _} ->
                    ?LOG("channel execution error while waiting for bridge"),
                    {error, execution_failure};
                {<<"asr">>, <<"asr_resp">>} ->
                    {ok, wh_json:get_value(<<"Response-Text">>, JObj)};
                {_,_} ->
                    asr_response()
            end
    end.

send_asr_info(Call, #dbn_state{asr_endpoint=EP, asr_account_id=AID, asr_account_pass=Pass, asr_lang=Lang}) ->
    Req = [{<<"ASR-Endpoint">>, EP}
           ,{<<"ASR-Account-ID">>, AID}
           ,{<<"ASR-Account-Password">>, Pass}
           ,{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue(Call)}
           ,{<<"Language">>, Lang}
           ,{<<"Stream-Response">>, false}
           | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_asr:publish_req(Req).

-spec collect_min_digits/5 :: (whapps_call:call(), #prompts{}, #dbn_state{}, lookup_table(), binary()) -> no_return().
collect_min_digits(Call, Prompts, #dbn_state{min_dtmf=MinDTMF, confirm_match=ConfirmMatch}=DbN, LookupTable, Collected) ->
    case collect_min_digits(MinDTMF - erlang:byte_size(Collected), Collected) of
        {ok, DTMFs} ->
            case analyze_dtmf(DTMFs, LookupTable) of
                no_results ->
                    ok = play_no_results(Call, Prompts),
                    start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table);
                {one_result, JObj} ->
                    case confirm_match(Call, Prompts, ConfirmMatch, JObj) of
                        true ->
                            route_to_match(Call, JObj);
                        false ->
                            start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
                    end;
                {many_results, Matches} ->
                    ok = play_has_matches(Call, Prompts, length(Matches)),
                    case matches_menu(Call, Prompts, Matches, whapps_call:account_db(Call)) of
                        {route, JObj} -> route_to_match(Call, JObj);
                        continue -> collect_next_dtmf(Call, Prompts, DbN, Matches);
                        start_over -> start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
                    end
            end;
        {error, timeout, DTMFs} ->
            {ok, PromptDTMFs} = play_min_digits_needed(Call, Prompts, MinDTMF),
            CurrDTMFs = <<DTMFs/binary, PromptDTMFs/binary>>,
            %% ?LOG("Failed to collect ~b digits (currently have ~s)", [MinDTMF, CurrDTMFs]),
            collect_min_digits(Call, Prompts, DbN, LookupTable, CurrDTMFs)
    end.

-spec analyze_dtmf/2 :: (binary(), wh_json:json_objects() | lookup_table()) -> 'no_dtmf' | 'no_results' | {'one_result', wh_json:json_object()} | {'many_results', wh_json:json_objects()}.
analyze_dtmf(<<>>, _) -> 'no_dtmf';
analyze_dtmf(DTMFs, LookupTable) ->
    ?LOG("Analyze: ~s", [DTMFs]),
    case lists:filter(fun(Entry) -> filter_table(Entry, DTMFs) end, LookupTable) of
        [] ->
            ?LOG("No results"),
            no_results;
        [{_ID, {_, JObj}}] ->
            ?LOG("Single result: ~s", [_ID]),
            {one_result, JObj};
        MatchesTable ->
            Matches = to_json_list(MatchesTable),
            ?LOG("Has more than 1 match"),
            {many_results, Matches}
    end.

to_json_list([{_, {_, _JObj}}|_] = MatchesTable) ->
    [ JObj || {_, {_, JObj}} <- MatchesTable ];
to_json_list(JObjs) ->
    JObjs.

-spec filter_table/2 :: (lookup_entry() | wh_json:json_object(), ne_binary()) -> boolean().
filter_table({_, {DialplanJObj, _}}, DTMFs) -> doc_matches(DialplanJObj, DTMFs);
filter_table(JObj, DTMFs) -> doc_matches(JObj, DTMFs).

%% Play instructions, then the prompts to scroll through messages
-spec matches_menu/4 :: (whapps_call:call(), #prompts{}, lookup_table() | wh_json:json_objects(), ne_binary()) -> {'route', wh_json:json_object()} | 'continue' | 'start_over'.
-spec matches_menu/5 :: (whapps_call:call(), #prompts{}, lookup_table() | wh_json:json_objects(), ne_binary(), non_neg_integer()) -> {'route', wh_json:json_object()} | 'continue' | 'start_over'.
-spec matches_menu_dtmf/6 :: (whapps_call:call(), #prompts{}, lookup_table() | wh_json:json_objects(), ne_binary(), non_neg_integer(), {'ok', binary()}) -> {'route', wh_json:json_object()} | 'continue' | 'start_over'.
matches_menu(Call, Prompts, LookupList, DB) ->
    matches_menu(Call, Prompts, LookupList, DB, 1).

matches_menu(Call, Prompts, [JObj|_] = LookupList, DB, MatchNo) ->
    case play_result(Call, Prompts, MatchNo, JObj, DB) of
        {ok, <<>>} ->
            case play_result_menu(Call, Prompts) of
                {ok, <<>>} -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
                {ok, _DTMF}=OK -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, OK)
            end;
        {ok, _DTMF}=OK -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, OK)
    end;
matches_menu(Call, Prompts, [], _, _) ->
    case play_no_more_results(Call, Prompts) of
        {ok, <<>>} -> no_matches_menu(Call, Prompts);
        {ok, _DTMF}=OK -> no_matches_dtmf(Call, Prompts, OK)
    end.

matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, {ok, <<>>}) ->
    matches_menu(Call, Prompts, LookupList, DB, MatchNo);
matches_menu_dtmf(_Call, _Prompts, [JObj | _], _DB, _MatchNo, {ok, ?DTMF_RESULT_CONNECT}) ->
    %% ?LOG("Routing to ~s (match no ~b)", [_ID, _MatchNo]),
    {route, JObj};
matches_menu_dtmf(Call, Prompts, [_|RestList], DB, MatchNo, {ok, ?DTMF_RESULT_NEXT}) ->
    matches_menu(Call, Prompts, RestList, DB, MatchNo+1);
matches_menu_dtmf(_Call, _Prompts, _LookupList, _DB, _MatchNo, {ok, ?DTMF_RESULT_CONTINUE}) -> continue;
matches_menu_dtmf(_Call, _Prompts, _LookupList, _DB, _MatchNo, {ok, ?DTMF_RESULT_START}) -> start_over;
matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, _) ->
    case play_invalid_key(Call, Prompts) of
        {ok, <<>>} -> matches_menu(Call, Prompts, LookupList, DB, MatchNo);
        {ok, _DTMF}=OK -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, OK)
    end.

-spec no_matches_menu/2 :: (whapps_call:call(), #prompts{}) -> 'continue' | 'start_over'.
no_matches_menu(Call, Prompts) ->
    case play_no_results_menu(Call, Prompts) of
        {ok, <<>>} -> no_matches_dtmf(Call, Prompts, whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
        {ok, _DTMF}=OK -> no_matches_dtmf(Call, Prompts, OK)
    end.

no_matches_dtmf(Call, Prompts, {ok, <<>>}) -> no_matches_menu(Call, Prompts);
no_matches_dtmf(_Call, _Prompts, {ok, ?DTMF_NO_RESULT_CONTINUE}) -> continue;
no_matches_dtmf(_Call, _Prompts, {ok, ?DTMF_NO_RESULT_START}) -> start_over;
no_matches_dtmf(Call, Prompts, _) ->
    case play_invalid_key(Call, Prompts) of
        {ok, <<>>} -> no_matches_menu(Call, Prompts);
        {ok, _DTMF}=OK -> no_matches_dtmf(Call, Prompts, OK)
    end.

-spec play_no_results_menu/2 :: (whapps_call:call(), #prompts{}) -> {'ok', binary()}.
play_no_results_menu(Call, #prompts{no_results_menu=NoResultsMenu}) ->
    play_and_collect([{play, NoResultsMenu}], Call).

-spec play_asr_instructions/3 :: (whapps_call:call(), #prompts{}, #dbn_state{}) -> 'ok'.
play_asr_instructions(Call, #prompts{asr_instructions=AsrInstructions}, DbN) ->
    NoopID = whapps_call_command:audio_macro([{play, AsrInstructions}], Call),

    ?LOG("Send asr amqp"),
    send_asr_info(Call, DbN),

    ?LOG("Waiting for prompt to finish"),
    {ok, _JObj} = whapps_call_command:wait_for_noop(NoopID),
    ok.

-spec play_no_more_results/2 :: (whapps_call:call(), #prompts{}) -> {'ok', binary()}.
play_no_more_results(Call, #prompts{no_more_results=NoMoreResults}) ->
    play_and_collect([{play, NoMoreResults}], Call).

-spec play_result_menu/2 :: (whapps_call:call(), #prompts{}) -> {'ok', binary()}.
play_result_menu(Call, #prompts{result_menu=ResultMenu}) ->
    play_and_collect([{play, ResultMenu}], Call).

-spec play_result/5 :: (whapps_call:call(), #prompts{}, integer(), wh_json:json_object(), ne_binary()) -> {'ok', binary()}.
play_result(Call, #prompts{result_number=ResultNumber}, MatchNo, JObj, DB) ->
    Name = case wh_json:get_value(<<"media_name_id">>, JObj) of
               undefined -> {say, <<$', (wh_json:get_binary_value(<<"full_name">>, JObj))/binary, $'>>};
               MediaID -> {play, <<$/, DB/binary, $/, MediaID/binary>>}
           end,

    play_and_collect([{play, ResultNumber}
                      ,{say, wh_util:to_binary(MatchNo), <<"number">>}
                      ,Name
                     ], Call).

%% Takes a user doc ID, creates the endpoint, and routes the call (ending the dialplan callflow as well)
-spec route_to_match/2 :: (whapps_call:call(), wh_json:json_object()) -> 'ok'.
route_to_match(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"callflow_id">>, JObj)) of
        {ok, CallflowJObj} ->
            ?LOG("Routing to Callflow: ~s", [wh_json:get_value(<<"_id">>, CallflowJObj)]),
            cf_exe:branch(wh_json:get_value(<<"flow">>, CallflowJObj), Call);
        _ ->
            ?LOG("Failed to find callflow: ~s", [wh_json:get_value(<<"callflow_id">>, JObj)]),
            cf_exe:continue(Call)
    end.

-spec confirm_match/4 :: (whapps_call:call(), #prompts{}, boolean(), wh_json:json_object()) -> boolean().
confirm_match(_Call, _Prompts, false, _) ->
    true;
confirm_match(Call, Prompts, true, JObj) ->
    case play_confirm_match(Call, Prompts, JObj) of
        {ok, <<>>} -> confirm_match_dtmf(Call, Prompts, JObj, whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
        {ok, _DTMF}=OK -> confirm_match_dtmf(Call, Prompts, JObj, OK)
    end.

-spec confirm_match_dtmf/4 :: (whapps_call:call(), #prompts{}, wh_json:json_object(), {'ok', binary()}) -> boolean().
confirm_match_dtmf(Call, Prompts, JObj, {ok, <<>>}) ->
    confirm_match(Call, Prompts, true, JObj);
confirm_match_dtmf(_Call, _Prompts, _JObj, {ok, ?DTMF_ACCEPT_MATCH}) -> true;
confirm_match_dtmf(_Call, _Prompts, _JObj, {ok, ?DTMF_REJECT_MATCH}) -> false;
confirm_match_dtmf(Call, Prompts, JObj, _) ->
    case play_invalid_key(Call, Prompts) of
        {ok, <<>>} -> confirm_match(Call, Prompts, true, JObj);
        {ok, _DTMF}=OK -> confirm_match_dtmf(Call, Prompts, JObj, OK)
    end.

-spec play_invalid_key/2 :: (whapps_call:call(), #prompts{}) -> {'ok', binary()}.
play_invalid_key(Call, #prompts{invalid_key=InvalidKey}) ->
    play_and_collect([{play, InvalidKey}], Call).

-spec play_has_matches/3 :: (whapps_call:call(), #prompts{}, non_neg_integer()) -> 'ok'.
play_has_matches(Call, #prompts{result_match=ResultMatch}, Matches) ->
    ok = play_and_wait([{say, wh_util:to_binary(Matches), <<"number">>}
                        ,{play, ResultMatch, ?ANY_DIGIT}
                       ], Call).

-spec play_confirm_match/3 :: (whapps_call:call(), #prompts{}, wh_json:json_object()) -> {'ok', binary()}.
play_confirm_match(Call, #prompts{confirm_menu=ConfirmMenu, found=Found}, JObj) ->
    play_and_collect([{play, Found}
                      ,{say, wh_json:get_value(<<"first_name">>, JObj)}
                      ,{say, wh_json:get_value(<<"last_name">>, JObj)}
                      ,{play, ConfirmMenu, ?ANY_DIGIT}
                     ], Call).

-spec play_no_results/2 :: (whapps_call:call(), #prompts{}) -> 'ok'.
play_no_results(Call, #prompts{no_matching_results=NoMatchingResults}) ->
    ok = play_and_wait([{play, NoMatchingResults}], Call).

-spec play_min_digits_needed/3 :: (whapps_call:call(), #prompts{}, pos_integer()) -> {'ok', binary()}.
play_min_digits_needed(Call, #prompts{specify_minimum=SpecifyMinimum, letters_of_name=LettersOfName}, MinDTMF) ->
    play_and_collect([{play, SpecifyMinimum, ?ANY_DIGIT}
                      ,{say, wh_util:to_binary(MinDTMF), <<"number">>}
                      ,{play, LettersOfName, ?ANY_DIGIT}
                     ], Call).

-spec play_and_wait/2 :: ([whapps_call_command:audio_macro_prompt(),...], whapps_call:call()) -> 'ok'.
play_and_wait(AudioMacro, Call) ->
    NoopID = whapps_call_command:audio_macro(AudioMacro, Call),
    {ok, _JObj} = whapps_call_command:wait_for_noop(NoopID),
    ok.

%% {'ok', <<>>} usually indicates a timeout waiting for DTMF
-spec play_and_collect/2 :: ([whapps_call_command:audio_macro_prompt(),...], whapps_call:call()) -> {'ok', binary()}.
-spec play_and_collect/3 :: ([whapps_call_command:audio_macro_prompt(),...], whapps_call:call(), non_neg_integer()) -> {'ok', binary()}.
play_and_collect(AudioMacro, Call) ->
    play_and_collect(AudioMacro, Call, 1).
play_and_collect(AudioMacro, Call, NumDigits) ->
    NoopID = whapps_call_command:audio_macro(AudioMacro, Call),
    ?LOG("NoopID: ~s", [NoopID]),
    whapps_call_command:collect_digits(NumDigits, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call).

%% collect DTMF digits individually until length of DTMFs is == MinDTMF
-spec collect_min_digits/2 :: (non_neg_integer(), binary()) -> {'ok', ne_binary()} | {'error', 'timeout', binary()}.
collect_min_digits(MinDTMF, DTMFs) when MinDTMF > 0 ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            {error, timeout, DTMFs};
        {ok, DTMF} ->
            collect_min_digits(MinDTMF-1, <<DTMFs/binary, DTMF/binary>>)
    end;
collect_min_digits(_, DTMFs) ->
    {ok, DTMFs}.

-spec collect_next_dtmf/4 :: (whapps_call:call(), #prompts{}, #dbn_state{}, wh_json:json_objects()) -> no_return().
collect_next_dtmf(Call, Prompts, #dbn_state{digits_collected=DTMFs}=DbN, LookupTable) ->
    case collect_min_digits(1, <<>>) of
        {ok, Digit} ->
            analyze_dtmf(<<DTMFs/binary, Digit/binary>>, LookupTable);
        {error, timeout, _} ->
            case play_continue_prompt(Call, Prompts) of
                {ok, <<>>} -> collect_next_dtmf(Call, Prompts, DbN, LookupTable);
                {ok, Digit} ->
                    analyze_dtmf(<<DTMFs/binary, Digit/binary>>, LookupTable)
            end
    end.

-spec play_continue_prompt/2 :: (whapps_call:call(), #prompts{}) -> {'ok', binary()}.
play_continue_prompt(Call, #prompts{please_continue=PleaseContinue}) ->
    play_and_collect([{play, PleaseContinue}], Call).

-spec play_start_instructions/3 :: (whapps_call:call(), #prompts{}, 'first' | 'last') -> {'ok', binary()}.
play_start_instructions(Call, #prompts{enter_person=EnterPerson, firstname=FName, lastname=LName}, SortBy) ->
    NamePrompt = case SortBy of first -> FName; last -> LName end,

    play_and_collect([{play, EnterPerson}
                      ,{play, NamePrompt}
                     ], Call).

%% {user_id, callflow_id, user_doc}
-spec build_lookup_table/1 :: ([{ne_binary(), ne_binary(), wh_json:json_object()},...]) -> lookup_table().
build_lookup_table([_|_]=Docs) ->
    orddict:to_list(orddict:from_list([ {UserID, convert_fields(CallflowID, UserDoc, ?FIELDS)} || {UserID, CallflowID, UserDoc} <- Docs ])).

%% create a json object containing only fields in Fields, and the values converted
%% to their dialpad equivalents
%% as well as the json object without the dialpad encoding
%% only convert the fields in Fields (and strip others out of the docs)
-spec convert_fields/3 :: (ne_binary(), wh_json:json_object(), [ne_binary(),...]) -> {wh_json:json_object(), wh_json:json_object()}.
convert_fields(CallflowID, UserDoc, Fields) ->
    {AlphaJObj, JObj} = lists:foldr(fun(Field, {DialpadJObj, JObj}) ->
                                            {wh_json:set_value(Field, cf_util:alpha_to_dialpad(wh_json:get_value(Field, UserDoc)), DialpadJObj)
                                             ,wh_json:set_value(Field, wh_json:get_value(Field, UserDoc), JObj)
                                            }
                                    end, {wh_json:new(), wh_json:new()}, Fields),

    Media = case wh_json:get_value([<<"media">>, <<"name">>], JObj) of
                undefined -> [];
                MediaID -> [{<<"media_name_id">>, MediaID}]
            end,

    JObj1 = wh_json:from_list([
                               {<<"callflow_id">>, CallflowID}
                               ,{<<"full_name">>, <<(wh_json:get_value(<<"first_name">>, JObj))/binary, " ", (wh_json:get_value(<<"last_name">>, JObj))/binary>>}
                               | Media
                              ]),

    {AlphaJObj, wh_json:merge_jobjs(JObj1, JObj)}.

%% Max DTMFs recorded is either 0 or at least Min
-spec get_max_dtmf/2 :: (non_neg_integer(), non_neg_integer()) -> integer().
get_max_dtmf(Max, Min) when Min > Max -> 0;
get_max_dtmf(Max, _) -> Max.

%% {user_id, callflow_id, user_doc}
-spec get_dir_docs/2 :: (ne_binary(), ne_binary()) -> [{ne_binary(), ne_binary(), wh_json:json_object()},...].
get_dir_docs(DirID, DBName) ->
    {ok, Docs} = couch_mgr:get_results(DBName, ?DIR_DOCS_VIEW, [{<<"key">>, DirID}, {<<"include_docs">>, true}]),
    [ {wh_json:get_value(<<"id">>, Doc), wh_json:get_value(<<"value">>, Doc), wh_json:get_value(<<"doc">>, Doc)} || Doc <- Docs ].

-spec get_sort_by/1 :: (ne_binary()) -> 'first' | 'last'.
get_sort_by(<<"first", _/binary>>) -> first;
get_sort_by(_) -> last.

%% does the eneterd DTMFs match either the first/last combo or the last/first combo?
-spec doc_matches/2 :: (wh_json:json_object(), ne_binary()) -> boolean().
doc_matches(JObj, DTMFs) ->
    FName = wh_json:get_value(<<"first_name">>, JObj),
    LName = wh_json:get_value(<<"last_name">>, JObj),

    field_matches(<<FName/binary, LName/binary>>, DTMFs) orelse
        field_matches(<<LName/binary, FName/binary>>, DTMFs).

%% does the enetered DTMFs match the field from the beginning?
-spec field_matches/2 :: (ne_binary(), ne_binary()) -> boolean().
field_matches(Field, DTMFs) ->
    case binary:match(Field, DTMFs) of
        {0, _} -> true;
        _ -> false
    end.
