{application, dth,
 [
  {description, "DTH - Integration with DTH Billing System"}
  ,{vsn, "0.1.0"}
  ,{modules, [dth_api, dth_app, dth_blacklist_req, dth_cdr_handler, dth, dth_listener, dth_sup, dth_util]}
  ,{registered, []}
  ,{applications, [
		   kernel
		   ,stdlib
		   ,crypto
		  ]}
  ,{mod, { dth_app, []} }
  ,{env, []}
 ]}.
