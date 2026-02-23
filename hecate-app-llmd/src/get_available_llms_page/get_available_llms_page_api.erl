%%% @doc API handler: GET /api/llm/models
%%%
%%% List all available LLM models from all configured providers.
%%% @end
-module(get_available_llms_page_api).

-export([init/2, routes/0]).

routes() -> [{"/api/llm/models", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_llmd_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    {ok, Models} = get_available_llms_page:list(),
    app_llmd_api_utils:json_ok(#{models => Models}, Req0).
