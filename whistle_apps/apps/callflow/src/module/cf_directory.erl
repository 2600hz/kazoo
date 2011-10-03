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
%%% @end
%%% Created : 20 Sep 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_directory).

-include("../callflow.hrl").

-export([handle/2]).

-define(DIR_DOCS_VIEW, <<"dialbyname/directory_docs_by_name">>).

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

-type lookup_entry() :: {ID :: binary(), {DialpadJObj :: json_object(), JObj :: json_object()}}.
-type lookup_table() :: [lookup_entry(),...] | [].

-record(dbn_state, {
	 sort_by = last :: first | last %% last_name field, or first_name field
         ,min_dtmf = 3 :: pos_integer() %% how many DTMF tones to collect before lookups start
         ,max_dtmf = 0 :: non_neg_integer() %% the most DTMF tones to collect; if max is reached, play choices. If max == 0, no limit
         ,confirm_match = false :: boolean() %% if false, once a match is made, connect the caller; if true, prompt for confirmation to connect
         ,digits_collected = <<>> :: binary() %% the digits collected from the caller so far
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
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> no_return() when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{call_id=CallId, account_db=AccountDB}=Call) ->
    put(callid, CallId),

    DirID = wh_json:get_value(<<"id">>, Data),
    {ok, DirJObj} = couch_mgr:open_doc(AccountDB, DirID),

    MinDTMF = wh_json:get_integer_value(<<"min_dtmf">>, DirJObj, 3),
    SortBy = get_sort_by(wh_json:get_value(<<"sort_by">>, DirJObj, <<"last_name">>)),
    LookupTable = build_lookup_table(get_dir_docs(DirID, AccountDB)),

    ?LOG_START("Dial By Name: ~s", [wh_json:get_value(<<"name">>, DirJObj)]),
    ?LOG("SortBy: ~s", [SortBy]),

    DbN = #dbn_state{
      sort_by = SortBy
      ,min_dtmf = MinDTMF
      ,max_dtmf = get_max_dtmf(wh_json:get_integer_value(<<"max_dtmf">>, DirJObj, 0), MinDTMF)
      ,confirm_match = wh_json:is_true(<<"confirm_match">>, DirJObj, false)
      ,orig_lookup_table = LookupTable
     },
    start_search(Call, #prompts{}, DbN, LookupTable).

-spec start_search/4 :: (Call, Prompts, State, LookupTable) -> no_return() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      State :: #dbn_state{},
      LookupTable :: lookup_table().
start_search(Call, Prompts, #dbn_state{sort_by=SortBy}=DbN, LookupTable) ->
    {ok, DTMFs} = play_start_instructions(Call, Prompts, SortBy),
    collect_min_digits(Call, Prompts, DbN, LookupTable, DTMFs).

-spec collect_min_digits/5 :: (Call, Prompts, State, LookupTable, Collected) -> no_return() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      State :: #dbn_state{},
      LookupTable :: lookup_table(),
      Collected :: binary().
collect_min_digits(Call, Prompts, #dbn_state{min_dtmf=MinDTMF}=DbN, LookupTable, Collected) ->
    case collect_min_digits(MinDTMF - erlang:byte_size(Collected), Collected) of
        {ok, DTMFs} ->
            analyze_dtmf(Call, Prompts, DbN#dbn_state{digits_collected=DTMFs}, LookupTable);
        {error, timeout, DTMFs} ->
            {ok, PromptDTMFs} = play_min_digits_needed(Call, Prompts, MinDTMF),
            CurrDTMFs = <<DTMFs/binary, PromptDTMFs/binary>>,
            %% ?LOG("Failed to collect ~b digits (currently have ~s)", [MinDTMF, CurrDTMFs]),
            collect_min_digits(Call, Prompts, DbN, LookupTable, CurrDTMFs)
    end.

-spec analyze_dtmf/4 :: (Call, Prompts, DbN, LookupTable) -> no_return() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      DbN :: #dbn_state{},
      LookupTable :: lookup_table().
analyze_dtmf(#cf_call{account_db=DB}=Call
             ,Prompts
             ,#dbn_state{digits_collected=DTMFs, confirm_match=ConfirmMatch}=DbN
             ,LookupTable) ->
    ?LOG("Analyze: ~s", [DTMFs]),
    case lists:filter(fun(Entry) -> filter_table(Entry, DTMFs) end, LookupTable) of
        [] ->
            ?LOG("No results"),
            ok = play_no_results(Call, Prompts),
            start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table);
        [{_ID, {_, JObj}}] ->
            ?LOG("Single result: ~s", [_ID]),
            case confirm_match(Call, Prompts, ConfirmMatch, JObj) of
                true ->
                    route_to_match(Call, JObj);
                false ->
                    start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
            end;
        Matches ->
            ?LOG("Has more than 1 match"),
            ok = play_has_matches(Call, Prompts, length(Matches)),
            case matches_menu(Call, Prompts, Matches, DB) of
                {route, JObj} -> route_to_match(Call, JObj);
                continue -> collect_next_dtmf(Call, Prompts, DbN, Matches);
                start_over -> start_search(Call, Prompts, DbN#dbn_state{digits_collected = <<>>}, DbN#dbn_state.orig_lookup_table)
            end
    end.

-spec filter_table/2 :: (Entry, DTMFs) -> boolean() when
      Entry :: lookup_entry(),
      DTMFs :: binary().
filter_table({_, {DialplanJObj, _}}, DTMFs) -> doc_matches(DialplanJObj, DTMFs).

%% Play instructions, then the prompts to scroll through messages
-spec matches_menu/4 :: (Call, Prompts, LookupList, DB) -> {'route', json_object()} | 'continue' | 'start_over' when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      LookupList :: lookup_table(),
      DB :: binary().
matches_menu(Call, Prompts, LookupList, DB) ->
    matches_menu(Call, Prompts, LookupList, DB, 1).

-spec matches_menu/5 :: (Call, Prompts, LookupList, DB, MatchNo) -> {'route', json_object()} | 'continue' | 'start_over' when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      LookupList :: lookup_table(),
      DB :: binary(),
      MatchNo :: non_neg_integer().
matches_menu(Call, Prompts, [{_,{_, JObj}}|_] = LookupList, DB, MatchNo) ->
    case play_result(Call, Prompts, MatchNo, JObj, DB) of
	{ok, <<>>} ->
	    case play_result_menu(Call, Prompts) of
		{ok, <<>>} -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, cf_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
		{ok, _DTMF}=OK -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, OK)
	    end;
	{ok, _DTMF}=OK -> matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, OK)
    end;
matches_menu(Call, Prompts, [], _, _) ->
    case play_no_more_results(Call, Prompts) of
	{ok, <<>>} -> no_matches_menu(Call, Prompts);
	{ok, _DTMF}=OK -> no_matches_dtmf(Call, Prompts, OK)
    end.

-spec matches_menu_dtmf/6 :: (Call, Prompts, LookupList, DB, MatchNo, CollectedDTMF) -> {'route', json_object()} | 'continue' | 'start_over' when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      LookupList :: lookup_table(),
      DB :: binary(),
      MatchNo :: integer(),
      CollectedDTMF :: {'ok', binary()}.
matches_menu_dtmf(Call, Prompts, LookupList, DB, MatchNo, {ok, <<>>}) ->
    matches_menu(Call, Prompts, LookupList, DB, MatchNo);
matches_menu_dtmf(_Call, _Prompts, [{_ID, {_, JObj}} | _], _DB, _MatchNo, {ok, ?DTMF_RESULT_CONNECT}) ->
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

-spec no_matches_menu/2 :: (Call, Prompts) -> 'continue' | 'start_over' when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
no_matches_menu(Call, Prompts) ->
    case play_no_results_menu(Call, Prompts) of
        {ok, <<>>} -> no_matches_dtmf(Call, Prompts, cf_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
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

-spec play_no_results_menu/2 :: (Call, Prompts) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_no_results_menu(Call, #prompts{no_results_menu=NoResultsMenu}) ->
    play_and_collect([{play, NoResultsMenu}], Call).

-spec play_no_more_results/2 :: (Call, Prompts) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_no_more_results(Call, #prompts{no_more_results=NoMoreResults}) ->
    play_and_collect([{play, NoMoreResults}], Call).

-spec play_result_menu/2 :: (Call, Prompts) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_result_menu(Call, #prompts{result_menu=ResultMenu}) ->
    play_and_collect([{play, ResultMenu}], Call).

-spec play_result/5 :: (Call, Prompts, MatchNo, JObj, DB) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      MatchNo :: integer(),
      JObj :: json_object(),
      DB :: binary().
play_result(Call, #prompts{result_number=ResultNumber}, MatchNo, JObj, DB) ->
    Name = case wh_json:get_value(<<"media_name_id">>, JObj) of
               undefined -> {say, <<$', (wh_json:get_binary_value(<<"full_name">>, JObj))/binary, $'>>};
               MediaID -> {play, <<$/, DB/binary, $/, MediaID/binary>>}
           end,

    play_and_collect([
		      {play, ResultNumber}
		      ,{say, wh_util:to_binary(MatchNo), <<"number">>}
		      ,Name
		     ], Call).

%% Takes a user doc ID, creates the endpoint, and routes the call (ending the dialplan callflow as well)
-spec route_to_match/2 :: (Call, JObj) -> {'branch', json_object()} | {'continue'} when
      Call :: #cf_call{},
      JObj :: json_object().
route_to_match(#cf_call{cf_pid=CFPid, account_db=DB}, JObj) ->
    case couch_mgr:open_doc(DB, wh_json:get_value(<<"callflow_id">>, JObj)) of
        {ok, CallflowJObj} ->
            ?LOG("Routing to Callflow: ~s", [wh_json:get_value(<<"_id">>, CallflowJObj)]),
            CFPid ! {branch, wh_json:get_value(<<"flow">>, CallflowJObj)};
        _ ->
            ?LOG("Failed to find callflow: ~s", [wh_json:get_value(<<"callflow_id">>, JObj)]),
            CFPid ! {continue}
    end.

-spec confirm_match/4 :: (Call, Prompts, ConfirmMatch, UserJObj) -> boolean() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      ConfirmMatch :: boolean(),
      UserJObj :: json_object().
confirm_match(_Call, _Prompts, false, _) ->
    true;
confirm_match(Call, Prompts, true, JObj) ->
    case play_confirm_match(Call, Prompts, JObj) of
        {ok, <<>>} -> confirm_match_dtmf(Call, Prompts, JObj, cf_call_command:wait_for_dtmf(?TIMEOUT_DTMF));
        {ok, _DTMF}=OK -> confirm_match_dtmf(Call, Prompts, JObj, OK)
    end.

-spec confirm_match_dtmf/4 :: (Call, Prompts, JObj, DTMFCapture) -> boolean() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      JObj :: json_object(),
      DTMFCapture :: {'ok', binary()}.
confirm_match_dtmf(Call, Prompts, JObj, {ok, <<>>}) ->
    confirm_match(Call, Prompts, true, JObj);
confirm_match_dtmf(_Call, _Prompts, _JObj, {ok, ?DTMF_ACCEPT_MATCH}) -> true;
confirm_match_dtmf(_Call, _Prompts, _JObj, {ok, ?DTMF_REJECT_MATCH}) -> false;
confirm_match_dtmf(Call, Prompts, JObj, _) ->
    case play_invalid_key(Call, Prompts) of
	{ok, <<>>} -> confirm_match(Call, Prompts, true, JObj);
	{ok, _DTMF}=OK -> confirm_match_dtmf(Call, Prompts, JObj, OK)
    end.

-spec play_invalid_key/2 :: (Call, Prompts) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_invalid_key(Call, #prompts{invalid_key=InvalidKey}) ->
    play_and_collect([
		      {play, InvalidKey}
		     ], Call).

-spec play_has_matches/3 :: (Call, Prompts, Matches) -> 'ok' when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      Matches :: non_neg_integer().
play_has_matches(Call, #prompts{result_match=ResultMatch}, Matches) ->
    ok = play_and_wait([
			{say, wh_util:to_binary(Matches), <<"number">>}
			,{play, ResultMatch, ?ANY_DIGIT}
		       ], Call).

-spec play_confirm_match/3 :: (Call, Prompts, JObj) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      JObj :: json_object().
play_confirm_match(Call, #prompts{confirm_menu=ConfirmMenu, found=Found}, JObj) ->
    play_and_collect([
                      {play, Found}
                      ,{say, wh_json:get_value(<<"first_name">>, JObj)}
                      ,{say, wh_json:get_value(<<"last_name">>, JObj)}
                      ,{play, ConfirmMenu, ?ANY_DIGIT}
                     ], Call).

-spec play_no_results/2 :: (Call, Prompts) -> 'ok' when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_no_results(Call, #prompts{no_matching_results=NoMatchingResults}) ->
    ok = play_and_wait([{play, NoMatchingResults}], Call).

-spec play_min_digits_needed/3 :: (Call, Prompts, MinDTMF) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      MinDTMF :: pos_integer().
play_min_digits_needed(Call, #prompts{specify_minimum=SpecifyMinimum, letters_of_name=LettersOfName}, MinDTMF) ->
    play_and_collect([
		      {play, SpecifyMinimum, ?ANY_DIGIT}
		      ,{say, wh_util:to_binary(MinDTMF), <<"number">>}
		      ,{play, LettersOfName, ?ANY_DIGIT}
		     ], Call).

-spec play_and_wait/2 :: (AudioMacro, Call) -> 'ok' when
      AudioMacro :: [cf_call_command:audio_macro_prompt(),...],
      Call :: #cf_call{}.
play_and_wait(AudioMacro, Call) ->
    NoopID = cf_call_command:audio_macro(AudioMacro, Call),
    {ok, _JObj} = cf_call_command:wait_for_noop(NoopID),
    ok.

%% {'ok', <<>>} usually indicates a timeout waiting for DTMF
-spec play_and_collect/2 :: (AudioMacro, Call) -> {'ok', binary()} when
      AudioMacro :: [cf_call_command:audio_macro_prompt(),...],
      Call :: #cf_call{}.
-spec play_and_collect/3 :: (AudioMacro, Call, NumDigits) -> {'ok', binary()} when
      AudioMacro :: [cf_call_command:audio_macro_prompt(),...],
      Call :: #cf_call{},
      NumDigits :: non_neg_integer().
play_and_collect(AudioMacro, Call) ->
    play_and_collect(AudioMacro, Call, 1).
play_and_collect(AudioMacro, Call, NumDigits) ->
    NoopID = cf_call_command:audio_macro(AudioMacro, Call),
    ?LOG("NoopID: ~s", [NoopID]),
    cf_call_command:collect_digits(NumDigits, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call).

%% collect DTMF digits individually until length of DTMFs is == MinDTMF
-spec collect_min_digits/2 :: (MinDTMF, DTMFs) -> {'ok', binary()} | {'error', 'timeout', binary()} when
      MinDTMF :: integer(),
      DTMFs :: binary().
collect_min_digits(MinDTMF, DTMFs) when MinDTMF > 0 ->
    case cf_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            {error, timeout, DTMFs};
        {ok, DTMF} ->
            collect_min_digits(MinDTMF-1, <<DTMFs/binary, DTMF/binary>>)
    end;
collect_min_digits(_, DTMFs) ->
    {ok, DTMFs}.

-spec collect_next_dtmf/4 :: (Call, Prompts, DbN, LookupTable) -> no_return() when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      DbN :: #dbn_state{},
      LookupTable :: lookup_table().
collect_next_dtmf(Call, Prompts, #dbn_state{digits_collected=DTMFs}=DbN, LookupTable) ->
    case collect_min_digits(1, <<>>) of
        {ok, Digit} ->
            analyze_dtmf(Call, Prompts, DbN#dbn_state{digits_collected = <<DTMFs/binary, Digit/binary>>}, LookupTable);
        {error, timeout, _} ->
            case play_continue_prompt(Call, Prompts) of
                {ok, <<>>} -> collect_next_dtmf(Call, Prompts, DbN, LookupTable);
                {ok, Digit} ->
                    analyze_dtmf(Call, Prompts, DbN#dbn_state{digits_collected = <<DTMFs/binary, Digit/binary>>}, LookupTable)
            end
    end.

-spec play_continue_prompt/2 :: (Call, Prompts) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{}.
play_continue_prompt(Call, #prompts{please_continue=PleaseContinue}) ->
    play_and_collect([{play, PleaseContinue}], Call).

-spec play_start_instructions/3 :: (Call, Prompts, SortBy) -> {'ok', binary()} when
      Call :: #cf_call{},
      Prompts :: #prompts{},
      SortBy :: 'first' | 'last'.
play_start_instructions(Call, #prompts{enter_person=EnterPerson, firstname=FName, lastname=LName}, SortBy) ->
    NamePrompt = case SortBy of first -> FName; last -> LName end,

    play_and_collect([{play, EnterPerson}
                      ,{play, NamePrompt}
                     ], Call).

-spec build_lookup_table/1 :: (Docs) -> lookup_table() when
      Docs :: [{binary(), binary(), json_object()},...].
build_lookup_table([_|_]=Docs) ->
    orddict:to_list(orddict:from_list([ {ID, convert_fields(Callflow, DocData, ?FIELDS)} || {ID, Callflow, DocData} <- Docs ])).

%% create a json object containing only fields in Fields, and the values converted
%% to their dialpad equivalents
%% as well as the json object without the dialpad encoding
%% only convert the fields in Fields (and strip others out of the docs)
-spec convert_fields/3 :: (Callflow, Doc, Fields) -> {json_object(), json_object()} when
      Callflow :: binary(),
      Doc :: json_object(),
      Fields :: [binary(),...].
convert_fields(Callflow, Doc, Fields) ->
    {AlphaJObj, JObj} = lists:foldr(fun(Field, {DialpadJObj, JObj}) ->
                                            {wh_json:set_value(Field, cf_util:alpha_to_dialpad(wh_json:get_value(Field, Doc)), DialpadJObj)
                                             ,wh_json:set_value(Field, wh_json:get_value(Field, Doc), JObj)
                                            }
                                    end, {wh_json:new(), wh_json:new()}, Fields),

    Media = case wh_json:get_value([<<"media">>, <<"name">>], JObj) of
                undefined -> [];
                MediaID -> [{<<"media_name_id">>, MediaID}]
            end,

    JObj1 = wh_json:from_list([
                               {<<"callflow">>, Callflow}
                               ,{<<"full_name">>, <<(wh_json:get_value(<<"first_name">>, JObj))/binary, " ", (wh_json:get_value(<<"last_name">>, JObj))/binary>>}
                               | Media
                              ]),

    {AlphaJObj, wh_json:merge_jobjs(JObj1, JObj)}.

%% Max DTMFs recorded is either 0 or at least Min
-spec get_max_dtmf/2 :: (Max, Min) -> integer() when
      Max :: non_neg_integer(),
      Min :: non_neg_integer().
get_max_dtmf(Max, Min) when Min > Max -> 0;
get_max_dtmf(Max, _) -> Max.

-spec get_dir_docs/2 :: (DirName, DBName) -> [{ID :: binary(), CallflowID :: binary(), Directory :: json_object()},...] when
      DirName :: binary(),
      DBName :: binary().
get_dir_docs(DirName, DBName) ->
    {ok, Docs} = couch_mgr:get_results(DBName, ?DIR_DOCS_VIEW, [{<<"key">>, DirName}, {<<"include_docs">>, true}]),
    [ {wh_json:get_value(<<"id">>, Doc), wh_json:get_value(<<"value">>, Doc), wh_json:get_value(<<"doc">>, Doc)} || Doc <- Docs ].

-spec get_sort_by/1 :: (binary()) -> 'first' | 'last'.
get_sort_by(<<"first", _/binary>>) -> first;
get_sort_by(_) -> last.

%% does the eneterd DTMFs match either the first/last combo or the last/first combo?
-spec doc_matches/2 :: (JObj, DTMFs) -> boolean() when
      JObj :: json_object(),
      DTMFs :: binary().
doc_matches(JObj, DTMFs) ->
    FName = wh_json:get_value(<<"first_name">>, JObj),
    LName = wh_json:get_value(<<"last_name">>, JObj),

    field_matches(<<FName/binary, LName/binary>>, DTMFs) orelse
        field_matches(<<LName/binary, FName/binary>>, DTMFs).

%% does the enetered DTMFs match the field from the beginning?
-spec field_matches/2 :: (Field, DTMFs) -> boolean() when
      Field :: binary(),
      DTMFs :: binary().
field_matches(Field, DTMFs) ->
    case binary:match(Field, DTMFs) of
        {0, _} -> true;
        _ -> false
    end.
