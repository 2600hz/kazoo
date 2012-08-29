{application,gen_smtp,
             [{description,"An erlang SMTP server/client framework"},
              {vsn,"0.1"},
              {modules,[binstr,gen_smtp_client,gen_smtp_server,
                        gen_smtp_server_session,mimemail,smtp_server_example,
                        smtp_util,socket]},
              {applications,[kernel,stdlib]}]}.
