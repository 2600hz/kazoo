{application,ibrowse,
             [{description,"HTTP client application"},
              {vsn,"%IBROWSE_VSN%"},
              {modules,[ibrowse,ibrowse_app,ibrowse_http_client,ibrowse_lb,
                        ibrowse_lib,ibrowse_sup,ibrowse_test]},
              {registered,[]},
              {applications,[kernel,stdlib,sasl]},
              {env,[]},
              {mod,{ibrowse_app,[]}}]}.
