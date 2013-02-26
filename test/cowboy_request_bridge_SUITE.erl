-module(cowboy_request_bridge_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

suite() -> [{timetrap,{seconds,30}}].
all() -> [{group, onrequest}].

groups() ->
    [
     {onrequest, [], [
		      onrequest,
		      lebel_request
		     ]}
    ].

%% for cowboy 0.8
init_per_suite(Config) ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto),
    ok.

init_per_group(onrequest, Config) ->
    Port = 33084,
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(onrequest, 100, [{port, Port}], [
								 {env, [{dispatch, init_dispatch()}]},
								 {max_keepalive, 50},
								 {onrequest, fun onrequest_hook/1},
								 {timeout, 500}
								]),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client} | Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

init_dispatch() ->
    cowboy_router:compile(
	%% {Host, list({Path, Handler, Opts})}
	[{"localhost", [
	    {'_', nitrogen_handler, []}
    ]}]).

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

onrequest(Config) ->
    Client = ?config(client, Config),
    URL = build_url("/", Config),
    ct:log("-> url ~p", [URL]),
    {ok, Client2} = cowboy_client:request(<<"GET">>, URL, Client),
    ct:log("-> request sent", []),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    ct:log("-> response sent", []),
    {ok, Body, _} = cowboy_client:response_body(Client3),
    ct:log("-> response Body ~p", [Body]),
    %% somewhere in the reply page we should have a string
    %% created by nitrogen page index.erl
    nomatch /= binary:match(Body, <<"some text in label for test">>),
    ok.

%% Hook for the above onrequest tests.
onrequest_hook(Req) -> Req.

lebel_request(Config) ->
    Client = ?config(client, Config),
    {ok, Client2} = cowboy_client:request(<<"GET">>, build_url("/label", Config), Client),
    ct:log("-> request sent", []),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    ct:log("-> response sent", []),
    {ok, Body, _} = cowboy_client:response_body(Client3),
    ct:log("-> response Body ~p", [Body]),
    %% somewhere in the reply page we should have a string from label
    %% created by nitrogen page index.erl
    nomatch /= binary:match(Body, <<"label test">>),
    ok.
