%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_inum).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([gen_numbers/3]).
-export([is_number_billable/1]).

-include("../wnm.hrl").

-define(WH_INUM,<<"numbers%2Finum">>).
-define(INUM_VIEW_FILE, <<"views/inum.json">>).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/3 :: (ne_binary(), pos_integer(), wh_proplist()) -> {'ok', wh_json:object()} |
                                                        {'error', 'non_available'}.
find_numbers(Number, Quantity, Opts) when size(Number) < 5 ->
    find_numbers(<<"+883", Number/binary>>, Quantity,Opts);
find_numbers(Number, Quantity,Opts) ->
	AccountId = props:get_value(<<"Account-ID">>, Opts),
	case find_numbers_in_account(Number, Quantity,AccountId) of
		{error, non_available}=A ->
			case wh_services:find_reseller_id(AccountId) of
				AccountId -> A;
				ResellerId -> find_numbers_in_account(Number, Quantity,ResellerId)
			end;							  
		R -> R
	end.
		
	
find_numbers_in_account(Number, Quantity,AccountId) ->	
    ViewOptions = [{<<"startkey">>, [AccountId,<<"available">>, Number]}
                  ,{<<"endkey">>, [AccountId,<<"available">>, <<Number/binary, "\ufff0">>]}
                  ,{<<"limit">>, Quantity}
				  ,'include_docs'
                  ],
    case couch_mgr:get_results(?WH_INUM, <<"numbers/status">>, ViewOptions) of
         {ok, []} -> 
             lager:debug("found no available inum numbers for account ~p",[AccountId]),
             {error, non_available};
         {ok, JObjs} ->
             lager:debug("found ~p available inum numbers for account ~p", [length(JObjs),AccountId]),
             {ok, format_numbers_resp(JObjs)};
          {error, _R}=E ->
             lager:debug("failed to lookup available local numbers: ~p", [_R]),
             E
    end.

format_numbers_resp(JObjs) ->
	Numbers= lists:foldl(
			   fun(JObj, Acc) ->
					   Doc = wh_json:get_value(<<"doc">>,JObj),
					   Props = props:filter_undefined([
													   {<<"number">>,wh_json:get_value(<<"_id">>,Doc)}
													  ,{<<"rate">>,wh_json:get_value(<<"rate">>,Doc,<<"1">>)}
													  ,{<<"activation_charge">>,wh_json:get_value(<<"activation_charge">>,Doc,<<"0">>)}
													  ]),
					   [{<<(wh_json:get_value(<<"_id">>,Doc))/binary>>,Props} | Acc]
			   end
						,[]	,JObjs),
	wh_json:from_list(Numbers).

-spec is_number_billable/1 :: (wnm_number()) -> 'true' | 'false'.
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(#number{number=Number,assign_to=AssignTo, state=State}=N) ->
	lager:debug("inum acquiring number ~p",[Number]),
	update_doc(Number,[{<<"pvt_number_state">>, State}
					  ,{<<"pvt_assigned_to">>,AssignTo}]),
	
	N.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number/1 :: (wnm_number()) -> wnm_number().
disconnect_number(#number{number=Number}=N) -> 
	lager:debug("inum disconnect number ~p",[Number]),
	update_doc(Number,[{<<"pvt_number_state">>, <<"available">>}
					  ,{<<"pvt_assigned_to">>,<<>>}]),
	
	N#number{state = <<"released">>, reserve_history=ordsets:new(),hard_delete=true}.


-spec gen_numbers/3 :: (ne_binary(), pos_integer() , pos_integer()) -> 'ok'.
gen_numbers(AccountId,<<"8835100",_/binary>>=Number,Quantity) when Quantity > 0 andalso size(Number) =:= 15 ->
	gen_numbers(AccountId,wh_util:to_integer(Number),wh_util:to_integer(Quantity));
	
gen_numbers(AccountId,Number,Quantity) when Quantity > 0 andalso is_integer(Number) andalso is_integer(Quantity) ->
	JObj = wh_json:set_values([{<<"_id">>,<<"+",(wh_util:to_binary(Number))/binary>>}
							  ,{<<"pvt_account_id">>,AccountId}
							  ,{<<"pvt_number_state">>,<<"available">>}
							  ,{<<"pvt_type">>,<<"number">>}
							   ], wh_json:new()),
	R = save_doc(JObj),
	lager:info("Number ~p/~p/~p",[Number,Quantity,R]),
	gen_numbers(AccountId,Number+1,Quantity-1);
gen_numbers(_A,_B,0=_C) ->
	'ok'.

save_doc(JObj) ->
	case couch_mgr:save_doc(?WH_INUM, JObj) of
		{error,not_found} ->
			create_inum_db(),
			save_doc(JObj);
		R -> R
	end.

update_doc(Number,UpdateProps) ->
	couch_mgr:update_doc(?WH_INUM, Number, UpdateProps).


create_inum_db() ->
	couch_mgr:db_create(?WH_INUM),
    _ = couch_mgr:revise_doc_from_file(?WH_INUM, 'whistle_apps', ?INUM_VIEW_FILE),
	'ok'.
