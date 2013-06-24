{application, callflow,
 [
  {description, "Callflow - traversing throught the tree..."}
  ,{vsn, "0.0.1"}
  ,{modules, [callflow_app, callflow, callflow_maintenance, callflow_sup, cf_attributes, cf_endpoint, cf_exe, cf_exe_sup, cf_listener, cf_route_req, cf_route_resume, cf_route_win, cf_shared_listener, cf_util, wapi_callflow,cf_acdc_agent, cf_acdc_member, cf_acdc_queue, cf_callflow, cf_call_forward, cf_conference, cf_device, cf_dialplan, cf_directory, cf_disa, cf_do_not_disturb, cf_dynamic_cid, cf_hotdesk, cf_intercom, cf_manual_presence, cf_menu, cf_offnet, cf_page_group, cf_park, cf_pivot, cf_play, cf_prepend_cid, cf_privacy, cf_receive_fax, cf_record_call, cf_resources, cf_response, cf_ring_group, cf_set, cf_temporal_route, cf_user, cf_voicemail]}
  ,{registered, []}
  ,{applications, [
		   kernel
		   ,stdlib
		   ,crypto
		  ]}
  ,{mod, { callflow_app, []} }
  ,{env, []}
 ]}.
