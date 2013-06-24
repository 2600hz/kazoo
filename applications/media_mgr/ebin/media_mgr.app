{application, media_mgr,
 [
  {description, "Media Manager - Stream media via Shout from Couch"}
  ,{vsn, "0.5.0"}
  ,{modules, [media_listener, media_mgr_app, media_mgr, media_mgr_sup]}
  ,{registered, []}
  ,{applications, [
		   kernel
		   ,stdlib
		   ,crypto
		  ]}
  ,{mod, { media_mgr_app, []} }
  ,{env, []}
 ]}.
