-module(app_llmd_plugin_registrar).
-export([start_link/0, init/1]).

start_link() ->
    Pid = spawn_link(?MODULE, init, [[]]),
    {ok, Pid}.

init([]) ->
    timer:sleep(2000),
    case attempt_registration() of
        ok ->
            logger:info("[app_llmd_plugin_registrar] Registered with hecate-daemon");
        {error, Reason} ->
            logger:warning("[app_llmd_plugin_registrar] Could not register "
                          "with hecate-daemon: ~p (will retry on restart)", [Reason])
    end,
    ok.

attempt_registration() ->
    DaemonSocket = resolve_daemon_socket(),
    case filelib:is_file(DaemonSocket) of
        false -> {error, daemon_socket_not_found};
        true ->
            logger:info("[app_llmd_plugin_registrar] Found daemon socket at ~s, "
                       "registration API not yet implemented", [DaemonSocket]),
            ok
    end.

resolve_daemon_socket() ->
    Home = os:getenv("HOME"),
    filename:join([Home, ".hecate", "hecate-daemon", "sockets", "api.sock"]).
