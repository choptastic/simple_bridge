-module(cowboy_request_bridge_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("cowboy/include/http.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

%% init_per_suite(Config) ->
%%     Config.

%% end_per_suite(_Config) ->
%%     ok.

all() ->
	[{group, onrequest}].

groups() ->
    [
     {onrequest, [], [
		     % method,
		      onrequest
		     ]}
    ].

init_per_suite(Config) ->
	application:start(inets),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(inets),
	ok.

init_per_group(onrequest, Config) ->
	Port = 33082,
	Transport = cowboy_tcp_transport,
	{ok, _} = cowboy:start_listener(onrequest, 100,
		Transport, [{port, Port}],
		cowboy_http_protocol, [
			{dispatch, init_dispatch(Config)},
			{max_keepalive, 50},
			{onrequest, fun onrequest_hook/1},
			{timeout, 500}
		]),
	{ok, Client} = cowboy_client:init([]),
	[{scheme, <<"http">>}, {port, Port}, {opts, []},
		{transport, Transport}, {client, Client}|Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

%% Dispatch configuration.
init_dispatch(Config) ->
    %% Dir = filename:join(?config(priv_dir, Config), "templates"),
    [{[<<"localhost">>], [{[], nitrogen_handler, []}]}].

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

onrequest(Config) ->
    Client = ?config(client, Config),
    {ok, Client2} = cowboy_client:request(<<"GET">>, build_url("/", Config), Client),
    ct:log("-> onrquest1, ~p", [Client2]),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    ct:log("-> onrquest2, ~p", [Client3]),
    %% {<<"server">>, <<"Serenity">>} = lists:keyfind(<<"server">>, 1, Headers),
    %% {ok, <<"http_handler">>, _} = cowboy_client:response_body(Client3).
    ok.

%% Hook for the above onrequest tests.
onrequest_hook(Req) ->
    ct:log("-> onrquest_hook, ~p", [Req]),
    Req.
    %% case cowboy_http_req:qs_val(<<"reply">>, Req) of
    %% 	{undefined, Req2} ->
    %% 	    {ok, Req3} = cowboy_http_req:set_resp_header(
    %% 			   'Server', <<"Serenity">>, Req2),
    %% 	    Req3
    %% 	    %% {_, Req2} ->
    %% 	    %% 	{ok, Req3} = cowboy_http_req:reply(
    %% 	    %% 		200, [], <<"replied!">>, Req2),
    %% 	    %% 	Req3
    %% end.
