-module(hecate_app_llmd_app).
-behaviour(application).

-include_lib("reckon_db/include/reckon_db.hrl").

-export([start/2, stop/1]).

-dialyzer({nowarn_function, start_llm_store/0}).

start(_StartType, _StartArgs) ->
    case application:get_env(hecate_app_llmd, enabled, true) of
        false ->
            logger:info("[hecate_app_llmd] Disabled by config"),
            {ok, spawn(fun() -> receive stop -> ok end end)};
        true ->
            ok = app_llmd_paths:ensure_layout(),
            ok = ensure_pg_scope(),
            ok = start_llm_store(),
            ok = start_cowboy(),
            logger:info("[hecate_app_llmd] Started, socket at ~s",
                        [app_llmd_paths:socket_path("api.sock")]),
            hecate_app_llmd_sup:start_link()
    end.

stop(_State) ->
    ok = cowboy:stop_listener(app_llmd_http),
    cleanup_socket(),
    ok.

ensure_pg_scope() ->
    case pg:start_link(hecate_app_llmd) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

start_llm_store() ->
    DataDir = app_llmd_paths:reckon_path("llm"),
    ok = filelib:ensure_path(DataDir),
    Config = #store_config{
        store_id = llm_store,
        data_dir = DataDir,
        mode = single,
        writer_pool_size = 5,
        reader_pool_size = 5,
        gateway_pool_size = 2,
        options = #{}
    },
    case reckon_db_sup:start_store(Config) of
        {ok, _Pid} ->
            logger:info("[hecate_app_llmd] llm_store ready"),
            ok;
        {error, {already_started, _Pid}} ->
            logger:info("[hecate_app_llmd] llm_store already running"),
            ok;
        {error, Reason} ->
            logger:error("[hecate_app_llmd] Failed to start llm_store: ~p", [Reason]),
            error({llm_store_start_failed, Reason})
    end.

start_cowboy() ->
    SocketPath = app_llmd_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath),
    Routes = [
        {"/health", app_llmd_health_api, []},
        {"/manifest", app_llmd_manifest_api, []},
        {"/api/llm/chat", chat_to_llm_api, []},
        {"/api/llm/models", get_available_llms_page_api, []},
        {"/api/llm/health", check_llm_health_api, []},
        {"/api/llm/providers", get_providers_page_api, []},
        {"/api/llm/providers/add", add_provider_api, []},
        {"/api/llm/providers/reload", reload_providers_api, []},
        {"/api/llm/providers/:name/remove", remove_provider_api, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    TransOpts = #{
        socket_opts => [{ifaddr, {local, SocketPath}}],
        num_acceptors => 5
    },
    ProtoOpts = #{
        env => #{dispatch => Dispatch},
        idle_timeout => 600000,
        request_timeout => 600000
    },
    {ok, _} = cowboy:start_clear(app_llmd_http, TransOpts, ProtoOpts),
    ok.

cleanup_socket() ->
    SocketPath = app_llmd_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath).

cleanup_socket_file(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            logger:warning("[hecate_app_llmd] Failed to remove socket ~s: ~p", [Path, Reason]),
            ok
    end.
