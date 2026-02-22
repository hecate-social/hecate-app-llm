-module(app_llmd_api_utils).
-export([json_response/3, json_reply/3, json_ok/2, json_ok/3, json_error/3]).
-export([format_error/1]).
-export([method_not_allowed/1, not_found/1, bad_request/2]).
-export([read_json_body/1]).
-export([get_field/2, get_field/3]).

json_response(StatusCode, Body, Req0) ->
    JsonBody = json:encode(Body),
    Req = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, JsonBody, Req0),
    {ok, Req, []}.

json_reply(StatusCode, Body, Req) -> json_response(StatusCode, Body, Req).

json_ok(Result, Req) -> json_response(200, maps:merge(#{ok => true}, Result), Req).
json_ok(StatusCode, Result, Req) -> json_response(StatusCode, maps:merge(#{ok => true}, Result), Req).

json_error(StatusCode, Reason, Req) ->
    json_response(StatusCode, #{ok => false, error => format_error(Reason)}, Req).

method_not_allowed(Req) -> json_error(405, <<"Method not allowed">>, Req).
not_found(Req)          -> json_error(404, <<"Not found">>, Req).
bad_request(Reason, Req) -> json_error(400, Reason, Req).

format_error(R) when is_binary(R) -> R;
format_error(R) when is_atom(R)   -> atom_to_binary(R);
format_error({Type, Details})     -> iolist_to_binary(io_lib:format("~p: ~p", [Type, Details]));
format_error(R)                   -> iolist_to_binary(io_lib:format("~p", [R])).

read_json_body(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try {ok, json:decode(Body), Req1}
    catch _:_ -> {error, invalid_json, Req1}
    end.

get_field(Key, Map) -> get_field(Key, Map, undefined).
get_field(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    maps:get(Key, Map, maps:get(BinKey, Map, Default));
get_field(Key, Map, Default) when is_binary(Key) ->
    maps:get(Key, Map, Default).
