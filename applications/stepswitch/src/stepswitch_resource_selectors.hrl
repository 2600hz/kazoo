-ifndef(STEPSWITCH_RESOURCE_SELECTORS_HRL).
-include("stepswitch.hrl").

-define(ALLOWED_RESOURCE_FIELDS,[{<<"weight_cost">>, 'get_resrc_weight'}
				,{<<"rules">>, 'get_resrc_rules'}
				,{<<"cid_rules">>, 'get_resrc_cid_rules'}
				,{<<"flags">>, 'get_resrc_flags'}
                                ]).

-define(STEPSWITCH_RESOURCE_SELECTORS_HRL, 'true').
-endif.
