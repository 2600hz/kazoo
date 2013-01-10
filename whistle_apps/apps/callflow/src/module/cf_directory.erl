%%%-------------------------------------------------------------------
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
%%% @contributors
%%%   James Aimonetti
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

-define(TIMEOUT_MIN_DTMF, 5000).
-define(TIMEOUT_DTMF, 2000).
-define(TIMEOUT_ENDPOINT, ?DEFAULT_TIMEOUT).

-define(PROMPT_ENTER_PERSON, <<"system_media/dir-enter_person">>). %% Please enter the first few letters of the person's
-define(PROMPT_FIRSTNAME, <<"system_media/dir-first_name">>). %% first name
-define(PROMPT_LASTNAME, <<"system_media/dir-last_name">>). %% last name
-define(PROMPT_AT_EXTENSION, <<"system_media/dir-at_extension">>). %% at extension
-define(PROMPT_SPECIFY_MINIMUM, <<"system_media/dir-specify_minimum">>). %% you need to specify a minimum of
-define(PROMPT_LETTERS_OF_NAME, <<"system_media/dir-letters_of_person_name">>). %% letters of the person's name
-define(PROMPT_NO_MATCHING_RESULTS, <<"system_media/dir-no_matching_results">>). %% There were no matching results
-define(PROMPT_PLEASE_TRY_AGAIN, <<"system_media/dir-please_try_again">>). %% Please try again
-define(PROMPT_RESULT_NUMBER, <<"system_media/dir-result_number">>). %% Result number
-define(PROMPT_START_NEW_SEARCH, <<"system_media/dir-start_new_search">>). %% To start a new search
-define(PROMPT_TO_SEARCH_BY, <<"system_media/dir-to_search_by">>). %% To search by
-define(PROMPT_FOR_NEXT, <<"system_media/dir-for_next">>). %% for the previous entry
-define(PROMPT_NO_MORE_RESULTS, <<"system_media/dir-no_more_results">>). %% no more results
-define(PROMPT_RESULT_MATCH, <<"system_media/dir-result_match">>). %% result matched your search
-define(PROMPT_TOO_MANY_RESULTS, <<"system_media/dir-too_many_result">>). %% your search returned too many results
-define(PROMPT_TO_SELECT_ENTRY, <<"system_media/dir-to_select_entry">>). %% to select this entry
-define(PROMPT_CONFIRM_MENU, <<"system_media/dir-confirm_menu">>). %% press 1 to connect. press 9 to start over.
-define(PROMPT_FOUND, <<"system_media/dir-found">>). %% found
-define(PROMPT_PLEASE_CONTINUE, <<"system_media/dir-please_continue">>). %% please continue entering letters
-define(PROMPT_INVALID_KEY, <<"system_media/dir-invalid_key">>). %% invalid key pressed
-define(PROMPT_RESULT_MENU, <<"system_media/dir-result_menu">>). %% press one to connect. press two for the next result. press three to continue searching. press four to start over.
-define(PROMPT_NO_RESULTS_MENU, <<"system_media/dir-no_results_menu">>). %% press one to continue searching. press two to start over.
-define(PROMPT_ASR_INSTRUCTIONS, <<"system_media/dir-asr_instructions">>). %% Please say the name of the party you would like to call

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------
-record(directory_user, {
          first_name :: ne_binary()
         ,last_name :: ne_binary()
         ,full_name :: ne_binary()
         ,first_last_keys :: ne_binary() % DTMF-version of first, last
         ,last_first_keys :: ne_binary() % DTMF-version of last, first
         ,callflow_id :: ne_binary() % what callflow to use on match
         ,name_audio_id :: ne_binary() | 'undefined' % pre-recorded audio of user's name
         }).
-type directory_user() :: #directory_user{}.
-type directory_users() :: [directory_user(),...].

-record(directory, {
          sort_by :: 'first' | 'last'
         ,min_dtmf :: pos_integer()
         ,max_dtmf :: non_neg_integer()
         ,confirm_match :: boolean()
         ,digits_collected :: binary()
         ,users :: directory_users()
         ,curr_users = [] :: directory_users() | []
         }).
-type directory() :: #directory{}.

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
    {ok, DirJObj} = couch_mgr:open_doc(whapps_call:account_db(Call), wh_json:get_value(<<"id">>, Data)),
    whapps_call_command:answer(Call),
    case get_directory_listing(whapps_call:account_db(Call), wh_json:get_value(<<"_id">>, DirJObj)) of
        {ok, Users} ->
            State = #directory{
              sort_by = get_sort_by(wh_json:get_value(<<"sort_by">>, DirJObj, <<"last_name">>))
              ,min_dtmf = wh_json:get_integer_value(<<"min_dtmf">>, DirJObj, 3)
              ,max_dtmf = wh_json:get_integer_value(<<"max_dtmf">>, DirJObj, 0)
              ,confirm_match = wh_json:is_true(<<"confirm_match">>, DirJObj, false)
              ,digits_collected = <<>>
              ,users = Users
             },
            _ = log(Users),
            directory_start(Call, State, Users);
        {error, no_users_in_directory} ->
            _ = play_no_users(Call),
            cf_exe:continue(Call);
        {error, _} ->
            _ = play_error(Call),
            cf_exe:continue(Call)
    end.

-spec directory_start/3 :: (whapps_call:call(), directory(), directory_users()) -> 'ok'.
directory_start(Call, State, CurrUsers) ->
    _ = whapps_call_command:flush_dtmf(Call),
    {ok, DTMF} = play_directory_instructions(Call, sort_by(State)),
    collect_min_digits(Call, add_dtmf(clear_dtmf(State), DTMF), CurrUsers, min_dtmf(State) - byte_size(DTMF)).

collect_min_digits(Call, State, CurrUsers, Min) when Min =< 0 ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            lager:info("waited enough, trying to match"),
            maybe_match(Call, State, CurrUsers);
        {ok, DTMF} ->
            lager:info("caller still entering DTMF, keep letting them"),
            collect_more_digits(Call, add_dtmf(State, DTMF), CurrUsers)
    end;
collect_min_digits(Call, State, CurrUsers, Min) ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_MIN_DTMF) of
        {ok, <<>>} ->
            lager:info("timeout waiting for a dtmf"),
            {ok, DTMF} = play_min_digits_needed(Call, min_dtmf(State)),
            collect_min_digits(Call, add_dtmf(clear_dtmf(State), DTMF), CurrUsers, Min - byte_size(DTMF));
        {ok, DTMF} ->
            collect_min_digits(Call, add_dtmf(State, DTMF), CurrUsers, Min-1)
    end.

collect_more_digits(Call, State, CurrUsers) ->
    case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
        {ok, <<>>} ->
            lager:info("failed to collect more digits, maybe_match"),
            maybe_match(Call, State, CurrUsers);
        {ok, DTMF} ->
            lager:info("recv more dtmf: ~s", [DTMF]),
            collect_more_digits(Call, add_dtmf(State, DTMF), CurrUsers)
    end.

maybe_match(Call, State, CurrUsers) ->
    case filter_users(CurrUsers, dtmf_collected(State)) of
        [] ->
            lager:info("no users left matching DTMF string"),
            _ = play_no_users(Call),
            directory_start(Call, clear_dtmf(State), users(State));
        [User] ->
            lager:info("one user found: ~s", [full_name(User)]),
            case maybe_confirm_match(Call, User, confirm_match(State)) of
                true ->
                    lager:info("match confirmed, routing"),
                    route_to_match(Call, callflow(Call, User));
                false ->
                    lager:info("match denied, starting over"),
                    directory_start(Call, clear_dtmf(State), users(State))
            end;
        Users ->
            lager:info("more than one match found"),
            matches_menu(Call, State, Users)
    end.

-spec matches_menu/3 :: (whapps_call:call(), directory(), directory_users()) -> 'ok'.
matches_menu(Call, State, Users) ->
    maybe_match_users(Call, save_current_users(State, Users), Users, 1).

maybe_match_users(Call, State, [], _) ->
    lager:info("failed to match any users, back to the beginning"),
    _ = play_no_users(Call),
    directory_start(Call, clear_dtmf(State), users(State));
maybe_match_users(Call, State, [U|Us], MatchNum) ->
    case maybe_match_user(Call, U, MatchNum) of
        route ->
            case maybe_confirm_match(Call, U, confirm_match(State)) of
                true ->
                    lager:info("match confirmed, routing"),
                    route_to_match(Call, callflow(Call, U));
                false ->
                    lager:info("match denied, continuing"),
                    maybe_match_users(Call, State, Us, MatchNum+1)
            end;
        next ->
            lager:info("moving to next user"),
            maybe_match_users(Call, State, Us, MatchNum+1);
        continue ->
            lager:info("caller wants to enter more DMTF"),
            collect_more_digits(Call, clear_current_users(State), get_current_users(State));
        start_over ->
            lager:info("starting over"),
            directory_start(Call, clear_dtmf(State), users(State));
        invalid ->
            lager:info("invalid key press"),
            _ = play_invalid(Call),
            maybe_match_users(Call, State, [U|Us], MatchNum)
    end.

-spec maybe_match_user/3 :: (whapps_call:call(), directory_user(), pos_integer()) -> 'route' |
                                                                                     'next' |
                                                                                     'start_over' |
                                                                                     'invalid' |
                                                                                     'continue'.
maybe_match_user(Call, U, MatchNum) ->
    UserName = case media_name(U) of
                   undefined -> {tts, <<$', (full_name(U))/binary, $'>>};
                   MediaID -> {play, <<$/, (whapps_call:account_db(Call))/binary
                                       ,$/, MediaID/binary>>}
               end,
    lager:info("playing username with: ~p", [UserName]),

    case play_user(Call, UserName, MatchNum) of
        {ok, <<>>} ->
            lager:info("nothing pressed during user prompts, wait for something"),
            case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
                {ok, <<>>} ->
                    maybe_match_user(Call, U, MatchNum);
                {ok, DTMF} ->
                    interpret_user_match_dtmf(DTMF)
            end;
        {ok, DTMF} ->
            interpret_user_match_dtmf(DTMF)
    end.

-spec interpret_user_match_dtmf/1 :: (ne_binary()) -> 'route' |
                                                      'next' |
                                                      'start_over' |
                                                      'invalid' |
                                                      'continue'.
interpret_user_match_dtmf(?DTMF_RESULT_CONNECT) -> route;
interpret_user_match_dtmf(?DTMF_RESULT_NEXT) -> next;
interpret_user_match_dtmf(?DTMF_RESULT_START) -> start_over;
interpret_user_match_dtmf(?DTMF_RESULT_CONTINUE) -> continue;
interpret_user_match_dtmf(_) -> invalid.

-spec maybe_confirm_match/3 :: (whapps_call:call(), directory_user(), boolean()) -> boolean().
maybe_confirm_match(_, _, false) -> true;
maybe_confirm_match(Call, User, true) ->
    _ = whapps_call_command:flush_dtmf(Call),
    case play_confirm_match(Call, User) of
        {ok, ?DTMF_ACCEPT_MATCH} -> true;
        _ -> false
    end.

route_to_match(Call, fail) ->
    cf_exe:continue(Call);
route_to_match(Call, Callflow) ->
    cf_exe:branch(wh_json:get_value(<<"flow">>, Callflow), Call).

%%------------------------------------------------------------------------------
%% Audio Prompts
%%------------------------------------------------------------------------------
play_user(Call, UsernameTuple, MatchNum) ->
    play_and_collect(Call, [{play, ?PROMPT_RESULT_NUMBER}
                            ,{say, wh_util:to_binary(MatchNum), <<"number">>}
                            ,UsernameTuple
                            ,{play, ?PROMPT_RESULT_MENU}
                           ]).

play_invalid(Call) ->
    whapps_call_command:audio_macro([{play, ?PROMPT_INVALID_KEY}], Call).

play_confirm_match(Call, User) ->
    UserName = case media_name(User) of
                   undefined -> {tts, <<$', (full_name(User))/binary, $'>>};
                   MediaID -> {play, <<$/, (whapps_call:account_db(Call))/binary
                                       ,$/, MediaID/binary>>}
               end,
    lager:info("playing confirm_match with username: ~p", [UserName]),

    play_and_collect(Call, [{play, ?PROMPT_FOUND}
                            ,UserName
                            ,{play, ?PROMPT_CONFIRM_MENU, ?ANY_DIGIT}
                           ]).

-spec play_min_digits_needed/2 :: (whapps_call:call(), pos_integer()) -> {'ok', binary()}.
play_min_digits_needed(Call, MinDTMF) ->
    play_and_collect(Call, [{play, ?PROMPT_SPECIFY_MINIMUM, ?ANY_DIGIT}
                            ,{say, wh_util:to_binary(MinDTMF), <<"number">>}
                            ,{play, ?PROMPT_LETTERS_OF_NAME, ?ANY_DIGIT}
                           ]).

-spec play_directory_instructions(whapps_call:call(), 'first' | 'last' | ne_binary()) -> {'ok', binary()}.
play_directory_instructions(Call, 'first') ->
    play_directory_instructions(Call, ?PROMPT_FIRSTNAME);
play_directory_instructions(Call, 'last') ->
    play_directory_instructions(Call, ?PROMPT_LASTNAME);
play_directory_instructions(Call, NamePrompt) ->
    play_and_collect(Call, [{play, ?PROMPT_ENTER_PERSON}
                            ,{play, NamePrompt}
                           ]).

-spec play_no_users/1 :: (whapps_call:call()) -> ne_binary(). % noop id
play_no_users(Call) ->
    whapps_call_command:audio_macro([{play, ?PROMPT_NO_MORE_RESULTS}], Call).

-spec play_error/1 :: (whapps_call:call()) -> ne_binary(). % noop id
play_error(Call) ->
    whapps_call_command:audio_macro([{play, ?PROMPT_NO_MORE_RESULTS}], Call).

-spec play_and_collect/2 :: (whapps_call:call(), [whapps_call_command:audio_macro_prompt(),...]) -> {'ok', binary()}.
-spec play_and_collect/3 :: (whapps_call:call(), [whapps_call_command:audio_macro_prompt(),...], non_neg_integer()) -> {'ok', binary()}.
play_and_collect(Call, AudioMacro) ->
    play_and_collect(Call, AudioMacro, 1).
play_and_collect(Call, AudioMacro, NumDigits) ->
    NoopID = whapps_call_command:audio_macro(AudioMacro, Call),
    lager:info("play and collect noopID: ~s", [NoopID]),
    whapps_call_command:collect_digits(NumDigits, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call).

%%------------------------------------------------------------------------------
%% Directory State Functions
%%------------------------------------------------------------------------------
sort_by(#directory{sort_by=SB}) ->
    SB.

min_dtmf(#directory{min_dtmf=Min}) ->
    Min.

dtmf_collected(#directory{digits_collected=Collected}) ->
    Collected.

confirm_match(#directory{confirm_match=CM}) ->
    CM.

users(#directory{users=Us}) ->
    Us.

-spec add_dtmf/2 :: (directory(), binary()) -> directory().
add_dtmf(#directory{digits_collected=Collected}=State, NewDTMFs) ->
    State#directory{digits_collected = <<Collected/binary, NewDTMFs/binary>>}.

-spec clear_dtmf/1 :: (directory()) -> directory().
clear_dtmf(#directory{}=State) ->
    State#directory{digits_collected = <<>>}.

save_current_users(#directory{}=State, Users) ->
    State#directory{curr_users=Users}.

get_current_users(#directory{curr_users=Us}) ->
    Us.

clear_current_users(#directory{}=State) ->
    State#directory{curr_users=[]}.

%%------------------------------------------------------------------------------
%% Directory User Functions
%%------------------------------------------------------------------------------
-spec callflow/2 :: (whapps_call:call(), directory_user()) -> wh_json:json_object() | 'fail'.
callflow(Call, #directory_user{callflow_id=CF}) ->
    case couch_mgr:open_doc(whapps_call:account_db(Call), CF) of
        {ok, JObj} -> JObj;
        {error, _E} ->
            lager:info("failed to find callflow ~s: ~p", [CF, _E]),
            fail
    end.

first_last_dtmfs(#directory_user{first_last_keys=FL}) ->
    FL.

last_first_dtmfs(#directory_user{last_first_keys=LF}) ->
    LF.

full_name(#directory_user{full_name=FN}) ->
    FN.

media_name(#directory_user{name_audio_id = ID}) ->
    ID.

%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------
-spec get_sort_by/1 :: (ne_binary()) -> 'first' | 'last'.
get_sort_by(<<"first", _/binary>>) -> first;
get_sort_by(_) -> last.

-spec get_directory_listing/2 :: (ne_binary(), ne_binary()) -> {'ok', directory_users()} |
                                                               {'error', term()}.
get_directory_listing(Db, DirId) ->
    case couch_mgr:get_results(Db, ?DIR_DOCS_VIEW, [{key, DirId}, include_docs]) of
        {ok, []} ->
            lager:info("no users have been assigned to directory ~s", [DirId]),
            %% play no users in this directory
            {error, no_users_in_directory};
        {ok, Users} ->
            {ok, [ get_directory_user(wh_json:get_value(<<"doc">>, U), wh_json:get_value(<<"value">>, U)) || U <- Users]};
        {error, _E}=E ->
            lager:info("failed to lookup users for directory ~s: ~p", [DirId, _E]),
            E
    end.

-spec get_directory_user/2 :: (wh_json:json_object(), ne_binary()) -> directory_user().
get_directory_user(U, CallflowId) ->
    First = wh_json:get_value(<<"first_name">>, U),
    Last = wh_json:get_value(<<"last_name">>, U),

    #directory_user{
                     first_name = First
                     ,last_name = Last
                     ,full_name = <<First/binary, " ", Last/binary>>
                     ,first_last_keys = cf_util:alpha_to_dialpad(<<First/binary, Last/binary>>)
                     ,last_first_keys = cf_util:alpha_to_dialpad(<<Last/binary, First/binary>>)
                     ,callflow_id = CallflowId
                     ,name_audio_id = wh_json:get_value(?RECORDED_NAME_KEY, U)
                   }.


filter_users(Users, DTMFs) ->
    lager:info("filtering users by ~s", [DTMFs]),
    Size = byte_size(DTMFs),

    [U || U <- Users,
          maybe_dtmf_matches(DTMFs, Size, first_last_dtmfs(U)) orelse
              maybe_dtmf_matches(DTMFs, Size, last_first_dtmfs(U))
    ].

-spec maybe_dtmf_matches/3 :: (ne_binary(), pos_integer(), ne_binary()) -> boolean().
maybe_dtmf_matches(_, 0, _) -> false;
maybe_dtmf_matches(_, Size, User) when byte_size(User) < Size -> false;
maybe_dtmf_matches(DTMFs, Size, User) ->
    lager:info("match ~s(~b) to ~s", [DTMFs, Size, User]),
    <<ToMatch:Size/binary, _/binary>> = User,
    ToMatch =:= DTMFs.

log(Users) ->
    [lager:info("user: ~s: ~s: ~s", [full_name(U), last_first_dtmfs(U), first_last_dtmfs(U)]) || U <- Users].
