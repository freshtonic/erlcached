{application, erlcached,
  [{description, "Erlcached: Memcached in Erlang/OTP"},
   {vsn, "0.1"},
   {modules, [erlcached_app, erlcached_supervisor, erlcached_server, erlcached_memcached_ascii_protocol]},
   {registered, [erlcached_server, erlcached_supervisor]},
   {applications, [kernel, stdlib]},
   {mod, {erlcached_app, []}},
   {env, [{ascii_protocol_port, 9898}]},
   {start_phases, []}
  ]}.
