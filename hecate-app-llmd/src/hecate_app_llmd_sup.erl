-module(hecate_app_llmd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [
        #{
            id => manage_providers,
            start => {manage_providers, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => detect_llms,
            start => {detect_llms, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => report_llm_status,
            start => {report_llm_status, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => app_llmd_plugin_registrar,
            start => {app_llmd_plugin_registrar, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
