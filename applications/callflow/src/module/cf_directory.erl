%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Present a directory menu to the caller.
%%%
%%% <strong>Basic flow of a directory call:</strong>
%%% <ol>
%%%   <li>Prompt: Please enter the first few letters of the person's
%%%     <ul><li>First entry in sort order (first or last name)</li></ul>
%%%   </li>
%%%
%%%   <li>Receive `MIN_DTMF' DTMF tones. If timeout occurs:
%%%     <ol>
%%%       <li>Prompt: You need to specify a minimum of
%%%         <ul>
%%%           <li>MIN_DTMF</li>
%%%           <li>Prompt: letters of the person's name</li>
%%%         </ul>
%%%       </li>
%%%       <li>Go back into main #2</li>
%%%     </ol>
%%%   </li>
%%%
%%%   <li>After receiving `MIN_DTMF', filter table</li>
%%%
%%%   <li>Go into a next DTMF wait loop:
%%%     <ul>
%%%       <li>If timeout, prompt with `#' of matches, option to hear matches or continue pressing keys</li>
%%%       <li>If continue, go into next DTMF wait loop</li>
%%%       <li>Else go to `play_matches'</li>
%%%     </ul>
%%%   </li>
%%%
%%%   <li>`play_matches': Plays `hd(matches)', options to hear more or connect or continue pressing keys</li>
%%% </ol>
%%%
%%% If the flag `asr_enabled' is set, send the An ASR AMQP request, wait for the ASR response, and use
%%% that for finding matches. It's more an all-or-nothing situation.
%%%
%%% The `asr_provider' key has the following properties:
%%% <dl>
%%%   <dt>`p_endpoint'</dt><dd>The endpoint to bridge to, e.g. `user_or_did@asr-server.com'</dd>
%%%   <dt>`p_account_id'</dt><dd>The client's account id for receiving the text back, e.g. `you@xmpp-server.com'</dd>
%%%   <dt>`p_account_pass'</dt><dd><strong>Optional: </strong>Password for the client's account</dd>
%%%   <dt>`p_lang'</dt><dd>The language code for the ASR provider, defaults to `us-EN'</dd>
%%% </dl>
%%%
%%% So, the process becomes:
%%% <ol>
%%%  <li>Prompt "Please say the name of the person you'd like to be connected to"</li>
%%%  <li>Send ASR request with `CallID', `ControlQ', and a response `Q'</li>
%%%  <li>Wait for ASR response with text of what was said</li>
%%%  <li>Find matches and iterate through the list, or go back to 1</li>
%%% </ol>
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_directory).

-behaviour(gen_cf_action).

-include_lib("callflow/src/callflow.hrl").

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

-define(PROMPT_ENTER_PERSON_LASTNAME, <<"dir-enter_person_lastname">>). %% Please enter the first few letters of the person's lastname
-define(PROMPT_ENTER_PERSON_FIRSTNAME, <<"dir-enter_person_firstname">>). %% Please enter the first few letters of the person's firstname
-define(PROMPT_ENTER_PERSON_NAME, <<"dir-enter_person_name">>). %% Please enter the first few letters of the person's name
-define(PROMPT_FIRSTNAME, <<"dir-first_name">>). %% first name
-define(PROMPT_LASTNAME, <<"dir-last_name">>). %% last name
-define(PROMPT_SPECIFY_MINIMUM, <<"dir-specify_minimum">>). %% You need to specify a minimum of two digits
-define(PROMPT_LETTERS_OF_NAME, <<"dir-letters_of_person_name">>). %% letters of the person's name
-define(PROMPT_NO_RESULTS_FOUND, <<"dir-no_results_found">>). %% No match found
-define(PROMPT_NO_MORE_RESULTS, <<"dir-no_more_results">>). %% no more results
-define(PROMPT_CONFIRM_MENU, <<"dir-confirm_menu">>). %% press 1. to start over press 3
-define(PROMPT_FOUND, <<"dir-found">>). %% One match found. To connect to
-define(PROMPT_INVALID_KEY, <<"dir-invalid_key">>). %% invalid key pressed
-define(PROMPT_RESULT_NUMBER, <<"dir-result_number">>). %% To call
-define(PROMPT_RESULT_MENU, <<"dir-result_menu">>). %% press one. For the next result press two. To start over press three

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------
-record(directory_user, {first_name :: kz_term:ne_binary()
                        ,last_name :: kz_term:ne_binary()
                        ,full_name :: kz_term:ne_binary()
                        ,first_last_keys :: kz_term:ne_binary() % DTMF-version of first, last
                        ,last_first_keys :: kz_term:ne_binary() % DTMF-version of last, first
                        ,callflow_id :: kz_term:ne_binary() % what callflow to use on match
                        ,name_audio_id :: kz_term:api_binary() % pre-recorded audio of user's name
                        }).
-type directory_user() :: #directory_user{}.
-type directory_users() :: [directory_user()].

-type search_field() :: 'first' | 'last' | 'both'.

-record(directory, {sort_by = 'last' :: 'first' | 'last'
                   ,search_fields = 'both' :: search_field()
                   ,min_dtmf :: pos_integer()
                   ,max_dtmf :: non_neg_integer()
                   ,confirm_match = 'false' :: boolean()
                   ,digits_collected = <<>> :: binary()
                   ,users = [] :: directory_users()
                   ,curr_users = [] :: directory_users()
                   }).
-type directory() :: #directory{}.

-type dtmf_action() :: 'route' | 'next' | 'start_over' | 'invalid' | 'continue'.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    {'ok', DirJObj} = kz_datamgr:open_cache_doc(kapps_call:account_db(Call)
                                               ,kz_json:get_ne_binary_value(<<"id">>, Data)
                                               ),
    kapps_call_command:answer(Call),

    case get_directory_listing(kapps_call:account_db(Call)
                              ,kz_doc:id(DirJObj)
                              )
    of
        {'ok', Users} ->
            State = #directory{sort_by = get_sort_by(kz_json:get_value(<<"sort_by">>, DirJObj, <<"last_name">>))
                              ,search_fields = get_search_fields(kz_json:get_value(<<"search_fields">>, DirJObj, <<"both">>))
                              ,min_dtmf = kz_json:get_integer_value(<<"min_dtmf">>, DirJObj, 3)
                              ,max_dtmf = kz_json:get_integer_value(<<"max_dtmf">>, DirJObj, 0)
                              ,confirm_match = kz_json:is_true(<<"confirm_match">>, DirJObj, 'false')
                              ,digits_collected = <<>>
                              ,users = Users
                              },
            Users1 = sort_users(Users, State#directory.sort_by),
            _ = log(Users1),
            directory_start(Call, State, Users1);
        {'error', 'no_users_in_directory'} ->
            _ = play_no_users_found(Call),
            cf_exe:continue(Call);
        {'error', _E} ->
            lager:debug("error getting directory listing: ~p", [_E]),
            _ = play_no_users_found(Call),
            cf_exe:continue(Call)
    end.

-spec directory_start(kapps_call:call(), directory(), directory_users()) -> 'ok'.
directory_start(Call, State, CurrUsers) ->
    directory_start(Call, State, CurrUsers, 3).

-spec directory_start(kapps_call:call(), directory(), directory_users(), non_neg_integer()) -> 'ok'.
directory_start(Call, _State, _CurrUsers, 0) ->
    lager:error("maximum try to collect digits"),
    _NoopId = kapps_call_command:audio_macro([{'prompt', ?PROMPT_SPECIFY_MINIMUM}
                                             ,{'prompt', ?PROMPT_NO_RESULTS_FOUND}
                                             ]
                                            ,Call
                                            ),
    cf_exe:stop(Call);
directory_start(Call, State, CurrUsers, Loop) ->
    _ = kapps_call_command:flush_dtmf(Call),
    case play_directory_instructions(Call, search_fields(State)) of
        {'ok', <<>>} -> directory_start(Call, State, CurrUsers, Loop - 1);
        {'ok', DTMF} -> collect_digits(Call, State, CurrUsers, DTMF);
        {'error', _Error} ->
            lager:error("failed to collect digits: ~p", [_Error]),
            cf_exe:stop(Call)
    end.

-spec collect_digits(kapps_call:call(), directory(), directory_users(), binary()) -> 'ok'.
collect_digits(Call, State, CurrUsers, DTMF) ->
    case kapps_call_command:collect_digits(100, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, Call) of
        {'error', _E} ->
            lager:error("failed to collect digits: ~p", [_E]),
            cf_exe:stop(Call);
        {'ok', <<>>} ->
            _NoopId = kapps_call_command:audio_macro([{'prompt', ?PROMPT_SPECIFY_MINIMUM}], Call),
            directory_start(Call, State, CurrUsers);
        {'ok', <<"0">>} ->
            lager:info("caller chose to return to the main menu"),
            cf_exe:continue(Call);
        {'ok', DTMFS} ->
            maybe_match(Call, add_dtmf(add_dtmf(State, DTMF), DTMFS), CurrUsers)
    end.

-spec maybe_match(kapps_call:call(), directory(), directory_users()) -> 'ok'.
maybe_match(Call, State, CurrUsers) ->
    case filter_users(CurrUsers, dtmf_collected(State), search_fields(State)) of
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

-spec matches_menu(kapps_call:call(), directory(), directory_users()) -> 'ok'.
matches_menu(Call, State, Users) ->
    maybe_match_users(Call, save_current_users(State, Users), Users, 1).

-spec maybe_match_users(kapps_call:call(), directory(), directory_users(), pos_integer()) -> 'ok'.
maybe_match_users(Call, State, [], _) ->
    lager:info("failed to match any users, back to the beginning"),
    _ = play_no_users(Call),
    directory_start(Call, clear_dtmf(State), users(State));
maybe_match_users(Call, State, [U|Us], MatchNum) ->
    case maybe_match_user(Call, U, MatchNum, 3) of
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

-spec maybe_match_user(kapps_call:call(), directory_user(), pos_integer(), non_neg_integer()) -> dtmf_action().
maybe_match_user(Call, _U, _MatchNum, 0) ->
    lager:info("maximum try to receive DTMF from caller, hanging up"),
    _ = play_no_users(Call),
    cf_exe:stop(Call);
maybe_match_user(Call, U, MatchNum, Loop) ->
    UserName = username_audio_macro(Call, U),
    lager:info("playing username with: ~p", [UserName]),

    case play_user(Call, UserName, MatchNum) of
        {'ok', <<>>} ->
            lager:info("nothing pressed during user prompts, wait for something"),
            case kapps_call_command:wait_for_dtmf(?TIMEOUT_DTMF) of
                {'ok', <<>>} -> maybe_match_user(Call, U, MatchNum, Loop - 1);
                {'ok', DTMF} -> interpret_user_match_dtmf(DTMF);
                {'error', 'timeout'} ->
                    lager:info("failed to receive DTMF from caller, try again"),
                    maybe_match_user(Call, U, MatchNum, Loop - 1);
                {'error', 'channel_hungup'} ->
                    lager:info("channel hungup, we're done"),
                    cf_exe:stop(Call)
            end;
        {'ok', DTMF} -> interpret_user_match_dtmf(DTMF);
        {'error', _R} ->
            lager:info("error playing username: ~p", [_R]),
            cf_exe:stop(Call)
    end.

-spec interpret_user_match_dtmf(kz_term:ne_binary()) -> dtmf_action().
interpret_user_match_dtmf(?DTMF_RESULT_CONNECT) -> 'route';
interpret_user_match_dtmf(?DTMF_RESULT_NEXT) -> 'next';
interpret_user_match_dtmf(?DTMF_RESULT_START) -> 'start_over';
interpret_user_match_dtmf(_) -> 'invalid'.

-spec maybe_confirm_match(kapps_call:call(), directory_user(), boolean()) -> boolean().
maybe_confirm_match(_, _, 'false') -> 'true';
maybe_confirm_match(Call, User, 'true') ->
    _ = kapps_call_command:flush_dtmf(Call),
    case play_confirm_match(Call, User) of
        {'ok', ?DTMF_ACCEPT_MATCH} -> 'true';
        _ -> 'false'
    end.

-spec route_to_match(kapps_call:call(), 'fail' | kz_json:object()) -> 'ok'.
route_to_match(Call, 'fail') -> cf_exe:continue(Call);
route_to_match(Call, Callflow) ->
    cf_exe:branch(kz_json:get_value(<<"flow">>, Callflow), Call).

%%------------------------------------------------------------------------------
%% Audio Prompts
%%------------------------------------------------------------------------------
-spec play_user(kapps_call:call(), kapps_call_command:audio_macro_prompt(), any()) ->
                       kapps_call_command:collect_digits_return().
play_user(Call, UsernameTuple, _MatchNum) ->
    play_and_collect(Call, [{'prompt', ?PROMPT_RESULT_NUMBER}
                           ,UsernameTuple
                           ,{'prompt', ?PROMPT_RESULT_MENU}
                           ]).

-spec play_invalid(kapps_call:call()) -> kz_term:ne_binary().
play_invalid(Call) ->
    kapps_call_command:audio_macro([{'prompt', ?PROMPT_INVALID_KEY}], Call).

-spec play_confirm_match(kapps_call:call(), directory_user()) ->
                                kapps_call_command:collect_digits_return().
play_confirm_match(Call, User) ->
    UserName = username_audio_macro(Call, User),
    lager:info("playing confirm_match with username: ~p", [UserName]),

    play_and_collect(Call, [{'prompt', ?PROMPT_FOUND}
                           ,UserName
                           ,{'prompt', ?PROMPT_CONFIRM_MENU}
                           ]).

-spec username_audio_macro(kapps_call:call(), directory_user()) -> kapps_call_command:audio_macro_prompt().
username_audio_macro(Call, User) ->
    case media_name(User) of
        'undefined' -> {'tts', full_name(User)};
        MediaID     -> maybe_play_media(Call, User, MediaID)
    end.

-spec maybe_play_media(kapps_call:call(), directory_user(), kz_term:api_binary()) ->
                              kapps_call_command:audio_macro_prompt().
maybe_play_media(Call, User, MediaId) ->
    AccountDb = kapps_call:account_db(Call),

    case kz_datamgr:open_cache_doc(AccountDb, MediaId) of
        {'ok', Doc} ->
            case kz_doc:attachments(Doc) of
                'undefined'  -> {'tts', full_name(User)};
                _ValidAttach -> {'play', <<$/, AccountDb/binary, $/, MediaId/binary>>}
            end;
        {'error', _} -> {'tts', full_name(User)}
    end.

-spec play_directory_instructions(kapps_call:call(), search_field()) ->
                                         {'ok', binary()} |
                                         {'error', atom()}.
play_directory_instructions(Call, 'first') ->
    play_and_collect(Call, [{'prompt', ?PROMPT_ENTER_PERSON_FIRSTNAME}]);
play_directory_instructions(Call, 'last') ->
    play_and_collect(Call, [{'prompt', ?PROMPT_ENTER_PERSON_LASTNAME}]);
play_directory_instructions(Call, 'both') ->
    play_and_collect(Call, [{'prompt', ?PROMPT_ENTER_PERSON_NAME}]).

-spec play_no_users(kapps_call:call()) -> kz_term:ne_binary(). % noop id
play_no_users(Call) ->
    kapps_call_command:audio_macro([{'prompt', ?PROMPT_NO_MORE_RESULTS}], Call).

-spec play_no_users_found(kapps_call:call()) -> kz_term:ne_binary(). % noop id
play_no_users_found(Call) ->
    kapps_call_command:audio_macro([{'prompt', ?PROMPT_NO_RESULTS_FOUND}], Call).

-spec play_and_collect(kapps_call:call(), kapps_call_command:audio_macro_prompts()) ->
                              {'ok', binary()} |
                              {'error', atom()}.
play_and_collect(Call, AudioMacro) ->
    play_and_collect(Call, AudioMacro, 1).

-spec play_and_collect(kapps_call:call(), kapps_call_command:audio_macro_prompts(), non_neg_integer()) ->
                              kapps_call_command:collect_digits_return().
play_and_collect(Call, AudioMacro, NumDigits) ->
    NoopID = kapps_call_command:audio_macro(AudioMacro, Call),
    lager:info("play and collect noopID: ~s", [NoopID]),
    kapps_call_command:collect_digits(NumDigits, ?TIMEOUT_DTMF, ?TIMEOUT_DTMF, NoopID, Call).

%%------------------------------------------------------------------------------
%% Directory State Functions
%%------------------------------------------------------------------------------
search_fields(#directory{search_fields=SF}) -> SF.
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
-spec callflow(kapps_call:call(), directory_user()) -> kz_json:object() | 'fail'.
callflow(Call, #directory_user{callflow_id=CF}) ->
    case kz_datamgr:open_doc(kapps_call:account_db(Call), CF) of
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
-spec get_sort_by(kz_term:ne_binary()) -> 'first' | 'last'.
get_sort_by(<<"first", _/binary>>) -> 'first';
get_sort_by(_) -> 'last'.

-spec get_search_fields(kz_term:ne_binary()) -> search_field().
get_search_fields(<<"both">>) -> 'both';
get_search_fields(<<"first", _/binary>>) -> 'first';
get_search_fields(_) -> 'last'.

-spec get_directory_listing(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', directory_users()} |
                                   {'error', any()}.
get_directory_listing(Db, DirId) ->
    case kz_datamgr:get_results(Db, ?DIR_DOCS_VIEW, [{'key', DirId}, 'include_docs']) of
        {'ok', []} ->
            lager:info("no users have been assigned to directory ~s", [DirId]),
            %% play no users in this directory
            {'error', 'no_users_in_directory'};
        {'ok', Users} ->
            {'ok', [get_directory_user(kz_json:get_value(<<"doc">>, U), kz_json:get_value(<<"value">>, U)) || U <- Users]};
        {'error', _E}=E ->
            lager:info("failed to lookup users for directory ~s: ~p", [DirId, _E]),
            E
    end.

-spec get_directory_user(kz_json:object(), kz_term:ne_binary()) -> directory_user().
get_directory_user(U, CallflowId) ->
    First = kz_json:get_value(<<"first_name">>, U),
    Last = kz_json:get_value(<<"last_name">>, U),

    #directory_user{first_name = First
                   ,last_name = Last
                   ,full_name = <<First/binary, " ", Last/binary>>
                   ,first_last_keys = cf_util:alpha_to_dialpad(<<First/binary, Last/binary>>)
                   ,last_first_keys = cf_util:alpha_to_dialpad(<<Last/binary, First/binary>>)
                   ,callflow_id = CallflowId
                   ,name_audio_id = kz_json:get_value(?RECORDED_NAME_KEY, U)
                   }.

-spec sort_users(directory_users(), 'first' | 'last') -> directory_users().
sort_users(Users, 'first') ->
    lists:sort(fun sort_by_first/2, Users);
sort_users(Users, 'last') ->
    lists:sort(fun sort_by_last/2, Users).

-spec sort_by_first(directory_user(), directory_user()) -> boolean().
sort_by_first(#directory_user{first_name=AFirst, last_name=ALast}
             ,#directory_user{first_name=AFirst, last_name=BLast}
             ) ->
    ALast < BLast;
sort_by_first(#directory_user{first_name=AFirst}
             ,#directory_user{first_name=BFirst}
             ) ->
    AFirst < BFirst.

-spec sort_by_last(directory_user(), directory_user()) -> boolean().
sort_by_last(#directory_user{first_name=AFirst, last_name=ALast}
            ,#directory_user{first_name=BFirst, last_name=ALast}
            ) ->
    AFirst < BFirst;
sort_by_last(#directory_user{last_name=ALast}
            ,#directory_user{last_name=BLast}
            ) ->
    ALast < BLast.

-spec filter_users(directory_users(), kz_term:ne_binary(), search_field()) -> directory_users().
filter_users(Users, DTMFs, FirstCheck) ->
    lager:info("filtering users by ~s", [DTMFs]),
    queue:to_list(queue_users(Users, DTMFs, FirstCheck)).

-spec queue_users(directory_users(), kz_term:ne_binary(), search_field()) -> queue:queue().
queue_users(Users, DTMFs, FirstCheck) ->
    Size = byte_size(DTMFs),
    lists:foldl(fun(User, Queue) ->
                        maybe_queue_user(User, Queue, DTMFs, Size, FirstCheck)
                end
               ,queue:new()
               ,Users
               ).

-spec maybe_queue_user(directory_user(), queue:queue(), kz_term:ne_binary(), pos_integer(), search_field()) ->
                              queue:queue().
maybe_queue_user(User, Queue, DTMFs, Size, 'both') ->
    case maybe_dtmf_matches(DTMFs, Size, first_check('first', User)) of
        'true' -> queue:in(User, Queue);
        'false' ->
            case maybe_dtmf_matches(DTMFs, Size, first_check('last', User)) of
                'true' -> queue:in(User, Queue);
                'false' -> Queue
            end
    end;

maybe_queue_user(User, Queue, DTMFs, Size, FirstCheck) ->
    case maybe_dtmf_matches(DTMFs, Size, first_check(FirstCheck, User)) of
        'true' -> queue:in(User, Queue);
        'false' -> Queue
    end.

-spec first_check('last' | 'first', directory_user()) -> kz_term:ne_binary().
first_check('last', User) ->
    last_first_dtmfs(User);
first_check('first', User) ->
    first_last_dtmfs(User).

-spec maybe_dtmf_matches(kz_term:ne_binary(), pos_integer(), kz_term:ne_binary()) -> boolean().
maybe_dtmf_matches(_, 0, _) -> 'false';
maybe_dtmf_matches(_, Size, User) when byte_size(User) < Size -> 'false';
maybe_dtmf_matches(DTMFs, Size, User) ->
    lager:info("match ~s(~b) to ~s", [DTMFs, Size, User]),
    <<ToMatch:Size/binary, _/binary>> = User,
    ToMatch =:= DTMFs.

log(Users) ->
    [lager:info("user: ~s: ~s: ~s", [full_name(U), last_first_dtmfs(U), first_last_dtmfs(U)]) || U <- Users].
