{application, sample,
 [{description, "MessagePack RPC sample application"},
  {vsn, "0.0.2"},
  {modules, 
   [sample, sample_app, sample_client, sample_srv, sample_sup]
  },
  {registered, []},
  {mod, {sample_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
