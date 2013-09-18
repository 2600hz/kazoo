%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
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
-define(DTMF_REJECT_MATCH, <<"3">>).

-define(DTMF_RESULT_CONNECT, <<"1">>).
-define(DTMF_RESULT_NEXT, <<"2">>).
-define(DTMF_RESULT_START, <<"3">>).

-define(TIMEOUT_MIN_DTMF, 5000).
-define(TIMEOUT_DTMF, 2000).
-define(TIMEOUT_ENDPOINT, ?DEFAULT_TIMEOUT_S).

-define(PROMPT_ENTER_PERSON_LASTNAME, <<"system_media/dir-enter_person_lastname">>). %% Please enter the first few letters of the person's lastname
-define(PROMPT_ENTER_PERSON_FIRSTNAME, <<"system_media/dir-enter_person_firstname">>). %% Please enter the first few letters of the person's firstname
-define(PROMPT_FIRSTNAME, <<"system_media/dir-first_name">>). %% first name
-define(PROMPT_LASTNAME, <<"system_media/dir-last_name">>). %% last name
-define(PROMPT_SPECIFY_MINIMUM, <<"system_media/dir-specify_minimum">>). %% You need to specify a minimum of two digits
-define(PROMPT_LETTERS_OF_NAME, <<"system_media/dir-letters_of_person_name">>). %% letters of the person's name
-define(PROMPT_NO_RESULTS_FOUND, <<"system_media/dir-no_results_found">>). %% No match found
-define(PROMPT_NO_MORE_RESULTS, <<"system_media/dir-no_more_results">>). %% no more results
-define(PROMPT_CONFIRM_MENU, <<"system_media/dir-confirm_menu">>). %% press 1. to start over press 3
-define(PROMPT_FOUND, <<"system_media/dir-found">>). %% One match found. To connect to
-define(PROMPT_INVALID_KEY, <<"system_media/dir-invalid_key">>). %% invalid key pressed
-define(PROMPT_RESULT_NUMBER, <<"system_media/dir-result_number">>). %% To call
-define(PROMPT_RESULT_MENU, <<"system_media/dir-result_menu">>). %% press one. For the next result press two. To start over press three

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
         ,name_audio_id :: api_binary() % pre-recorded audio of user's name
         }).
-type directory_user() :: #directory_user{}.
-type directory_users() :: [directory_user(),...] | [].

-record(directory, {
          sort_by :: 'first' | 'last'
         ,min_dtmf :: pos_integer()
         ,max_dtmf :: non_neg_integer()
         ,confirm_match :: boolean()
         ,digits_collected :: binary()
         ,users :: directory_users()
         ,curr_users = [] :: directory_users()
         }).
-type directory() :: #directory{}.

-type dtmf_action() :: 'route' | 'next' | 'start_over' | 'invalid' | 'continue'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {'ok', DirJObj} = couch_mgr:open_doc(whapps_call:account_db(Call), wh_json:get_value(<<"id">>, Data)),
    whapps_call_command:answer(Call),

    case get_directory_listing(whapps_call:account_db(Call), wh_json:get_value(<<"_id">>, DirJObj)) of
        {'ok', Users} ->
            State = #directory{
              sort_by = get_sort_by(wh_json:get_value(<<"sort_by">>, DirJObj, <<"last_name">>))
              ,min_dtmf = wh_json:get_integer_value(<<"min_dtmf">>, DirJObj, 3)
              ,max_dtmf = wh_json:get_integer_value(<<"max_dtmf">>, DirJObj, 0)
              ,confirm_match = wh_json:is_true(<<"confirm_match">>, DirJObj, 'false')
              ,digits_collected = <<>>
              ,users = Users
             },
            _ = log(Users),
            directory_start(Call, State, Users);
        {'error', 'no_users_in_directory'} ->
            _ = play_no_users_found(Call),
            cf_exe:continue(Call);
        {'error', _E} ->
            lager:debug("error getting directory listing: ~p", [_E]),
            _ = play_no_users_found(Call),
            cf_exe:continue(Call)
    end.

-spec directory_start(whapps_call:call(), directory(), directory_users()) -> 'ok'.
directory_start(Call, State, CurrUsers) ->
    _ = whapps_call_command:flush_dtmf(Call),
    {'ok', DTMF} = play_directory_instructions(Call, sort_by(State)),
    case whapps_call_command:collect_digits(100, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, Call) of
        {'error', _E} ->
            lager:error("failed to collect digits: ~p", [_E]),
            cf_exe:stop(Call);
        {'ok', <<>>} ->
            whapps_call_command:audio_macro([{'play', ?PROMPT_SPECIFY_MINIMUM}], Call),
            directory_start(Call, State, CurrUsers);
        {'ok', DTMFS} ->
            maybe_match(Call, add_dtmf(add_dtmf(State, DTMF), DTMFS), CurrUsers)
    end.

maybe_match(Call, State, CurrUsers) ->
    case filter_users(CurrUsers, dtmf_collected(State)) of
        [] ->
            lager:info("no users left matching DTMF string"),
            _ = play_no_users_found(Call),
            directory_start(Call, clear_dtmf(State), users(State));
        [User] ->
            lager:info("one user found: ~s", [full_name(User)]),
            case maybe_confirm_match(Call, User, confirm_match(State)) of
                'true' ->
                    lager:info("match confirmed, routing"),
                    route_to_match(Call, callflow(Call, User));
                'false' ->
                    lager:info("match denied, starting over"),
                    directory_start(Call, clear_dtmf(State), users(State))
            end;
        Users ->
            lager:info("more than one match found"),
            matches_menu(Call, State, Users)
    end.

-spec matches_menu(whapps_call:call(), directory(), directory_users()) -> 'ok'.
matches_menu(Call, State, Users) ->
    maybe_match_users(Call, save_current_users(State, Users), Users, 1).

maybe_match_users(Call, State, [], _) ->
    lager:info("failed to match any users, back to the beginning"),
    _ = play_no_users(Call),
    directory_start(Call, clear_dtmf(State), users(State));
maybe_match_users(Call, State, [U|Us], MatchNum) ->
    case maybe_match_user(Call, U, MatchNum) of
        'route' ->
            route_to_match(Call, callflow(Call, U));
        'next' ->
            lager:info("moving to next user"),
            maybe_match_users(Call, State, Us, MatchNum+1);
        'start_over' ->
            lager:info("starting over"),
            directory_start(Call, clear_dtmf(State), users(State));
        'invalid' ->
            lager:info("invalid key press"),
            _ = play_invalid(Call),
            maybe_match_users(Call, State, [U|Us], MatchNum)
    end.

-spec maybe_match_user(whapps_call:call(), directory_user(), pos_integer()) -> dtmf_action().
maybe_match_user(Call, U, MatchNum) ->
    UserName = case media_name(U) of
                   'undefined' -> {'tts', <<39, (full_name(U))/binary, 39>>}; % 39 is ascii '
                   MediaID ->
                       {'play', <<$/, (whapps_call:account_db(Call))/binary, $/, MediaID/binary>>}
               end,
    lager:info("playing username with: ~p", [UserName]),

    case play_user(Call, UserName, MatchNum) of
        {'ok', <<>>} ->
            lager:info("nothing pressed during user prompts, wait for something"),
            case whapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
                {'ok', <<>>} -> maybe_match_user(Call, U, MatchNum);
                {'ok', DTMF} -> interpret_user_match_dtmf(DTMF)
            end;
        {'ok', DTMF} -> interpret_user_match_dtmf(DTMF)
    end.

-spec interpret_user_match_dtmf(ne_binary()) -> dtmf_action().
interpret_user_match_dtmf(?DTMF_RESULT_CONNECT) -> 'route';
interpret_user_match_dtmf(?DTMF_RESULT_NEXT) -> 'next';
interpret_user_match_dtmf(?DTMF_RESULT_START) -> 'start_over';
interpret_user_match_dtmf(_) -> 'invalid'.

-spec maybe_confirm_match(whapps_call:call(), directory_user(), boolean()) -> boolean().
maybe_confirm_match(_, _, 'false') -> 'true';
maybe_confirm_match(Call, User, 'true') ->
    _ = whapps_call_command:flush_dtmf(Call),
    case play_confirm_match(Call, User) of
        {'ok', ?DTMF_ACCEPT_MATCH} -> 'true';
        _ -> 'false'
    end.

route_to_match(Call, 'fail') -> cf_exe:continue(Call);
route_to_match(Call, Callflow) ->
    cf_exe:branch(wh_json:get_value(<<"flow">>, Callflow), Call).

%%------------------------------------------------------------------------------
%% Audio Prompts
%%------------------------------------------------------------------------------
play_user(Call, UsernameTuple, _MatchNum) ->
    play_and_collect(Call, [{'play', ?PROMPT_RESULT_NUMBER}
                            ,UsernameTuple
                            ,{'play', ?PROMPT_RESULT_MENU}
                           ]).

play_invalid(Call) ->
    whapps_call_command:audio_macro([{'play', ?PROMPT_INVALID_KEY}], Call).

play_confirm_match(Call, User) ->
    UserName =
        case media_name(User) of
            'undefined' -> {'tts', <<39, (full_name(User))/binary, 39>>}; %% 39 is ascii '
            MediaID -> {'play', <<$/, (whapps_call:account_db(Call))/binary, $/, MediaID/binary>>}
        end,
    lager:info("playing confirm_match with username: ~p", [UserName]),

    play_and_collect(Call, [{'play', ?PROMPT_FOUND}
                            ,UserName
                            ,{'play', ?PROMPT_CONFIRM_MENU, ?ANY_DIGIT}
                           ]).

-spec play_directory_instructions(whapps_call:call(), 'first' | 'last' | ne_binary()) ->
                                         {'ok', binary()}.
play_directory_instructions(Call, 'first') ->
    play_and_collect(Call, [{'play', ?PROMPT_ENTER_PERSON_FIRSTNAME}]);
play_directory_instructions(Call, 'last') ->
    play_and_collect(Call, [{'play', ?PROMPT_ENTER_PERSON_LASTNAME}]).

-spec play_no_users(whapps_call:call()) -> ne_binary(). % noop id
play_no_users(Call) ->
    whapps_call_command:audio_macro([{'play', ?PROMPT_NO_MORE_RESULTS}], Call).

-spec play_no_users_found(whapps_call:call()) -> ne_binary(). % noop id
play_no_users_found(Call) ->
    whapps_call_command:audio_macro([{'play', ?PROMPT_NO_RESULTS_FOUND}], Call).

-spec play_and_collect(whapps_call:call(), whapps_call_command:audio_macro_prompts()) ->
                              {'ok', binary()}.
-spec play_and_collect(whapps_call:call(), whapps_call_command:audio_macro_prompts(), non_neg_integer()) ->
                              {'ok', binary()}.
play_and_collect(Call, AudioMacro) ->
    play_and_collect(Call, AudioMacro, 1).
play_and_collect(Call, AudioMacro, NumDigits) ->
    NoopID = whapps_call_command:audio_macro(AudioMacro, Call),
    lager:info("play and collect noopID: ~s", [NoopID]),
    whapps_call_command:collect_digits(NumDigits, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call).

%%------------------------------------------------------------------------------
%% Directory State Functions
%%------------------------------------------------------------------------------
sort_by(#directory{sort_by=SB}) -> SB.
dtmf_collected(#directory{digits_collected=Collected}) -> Collected.
confirm_match(#directory{confirm_match=CM}) -> CM.
users(#directory{users=Us}) -> Us.

-spec add_dtmf(directory(), binary()) -> directory().
add_dtmf(#directory{digits_collected=Collected}=State, NewDTMFs) ->
    State#directory{digits_collected = <<Collected/binary, NewDTMFs/binary>>}.

-spec clear_dtmf(directory()) -> directory().
clear_dtmf(#directory{}=State) -> State#directory{digits_collected = <<>>}.

save_current_users(#directory{}=State, Users) -> State#directory{curr_users=Users}.

%%------------------------------------------------------------------------------
%% Directory User Functions
%%------------------------------------------------------------------------------
-spec callflow(whapps_call:call(), directory_user()) -> wh_json:object() | 'fail'.
callflow(Call, #directory_user{callflow_id=CF}) ->
    case couch_mgr:open_doc(whapps_call:account_db(Call), CF) of
        {'ok', JObj} -> JObj;
        {'error', _E} ->
            lager:info("failed to find callflow ~s: ~p", [CF, _E]),
            'fail'
    end.

first_last_dtmfs(#directory_user{first_last_keys=FL}) -> FL.
last_first_dtmfs(#directory_user{last_first_keys=LF}) -> LF.
full_name(#directory_user{full_name=FN}) -> FN.
media_name(#directory_user{name_audio_id = ID}) -> ID.

%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------
-spec get_sort_by(ne_binary()) -> 'first' | 'last'.
get_sort_by(<<"first", _/binary>>) -> 'first';
get_sort_by(_) -> 'last'.

-spec get_directory_listing(ne_binary(), ne_binary()) ->
                                   {'ok', directory_users()} |
                                   {'error', term()}.
get_directory_listing(Db, DirId) ->
    case couch_mgr:get_results(Db, ?DIR_DOCS_VIEW, [{'key', DirId}, 'include_docs']) of
        {'ok', []} ->
            lager:info("no users have been assigned to directory ~s", [DirId]),
            %% play no users in this directory
            {'error', 'no_users_in_directory'};
        {'ok', Users} ->
            {'ok', [get_directory_user(wh_json:get_value(<<"doc">>, U), wh_json:get_value(<<"value">>, U)) || U <- Users]};
        {'error', _E}=E ->
            lager:info("failed to lookup users for directory ~s: ~p", [DirId, _E]),
            E
    end.

-spec get_directory_user(wh_json:object(), ne_binary()) -> directory_user().
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

-spec maybe_dtmf_matches(ne_binary(), pos_integer(), ne_binary()) -> boolean().
maybe_dtmf_matches(_, 0, _) -> 'false';
maybe_dtmf_matches(_, Size, User) when byte_size(User) < Size -> 'false';
maybe_dtmf_matches(DTMFs, Size, User) ->
    lager:info("match ~s(~b) to ~s", [DTMFs, Size, User]),
    <<ToMatch:Size/binary, _/binary>> = User,
    ToMatch =:= DTMFs.

log(Users) ->
    [lager:info("user: ~s: ~s: ~s", [full_name(U), last_first_dtmfs(U), first_last_dtmfs(U)]) || U <- Users].
