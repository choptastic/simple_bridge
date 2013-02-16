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


%%for cowboy 0.6
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
    [{[<<"localhost">>], [{[], nitrogen_handler, []}]}].





%% %% for cowboy 0.8
%% init_per_suite(Config) ->
%%     application:start(crypto),
%%     application:start(ranch),
%%     application:start(cowboy),
%%     Config.

%% end_per_suite(_Config) ->
%%     application:stop(cowboy),
%%     application:stop(ranch),
%%     application:stop(crypto),
%%     ok.

%% init_per_group(onrequest, Config) ->
%%     Port = 33084,
%%     Transport = ranch_tcp,
%%     {ok, _} = cowboy:start_http(onrequest, 100, [{port, Port}], [
%% 								 {env, [{dispatch, init_dispatch(Config)}]},
%% 								 {max_keepalive, 50},
%% 								 {onrequest, fun onrequest_hook/1},
%% 								 {timeout, 500}
%% 								]),
%%     {ok, Client} = cowboy_client:init([]),
%%     [{scheme, <<"http">>}, {port, Port}, {opts, []},
%%      {transport, Transport}, {client, Client}|Config];

%% end_per_group(Name, _) ->
%%     cowboy:stop_listener(Name),
%%     ok.


%% init_dispatch(Config) ->
%%     cowboy_router:compile([
%% 			   {"localhost", [
%% 					  {"/", nitrogen_handler, []}
%% 					 ]}
%% 			  ]).

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

onrequest(Config) ->
    Client = ?config(client, Config),
    {ok, Client2} = cowboy_client:request(<<"GET">>, build_url("/", Config), Client),
    {ok, 200, Headers, Client3} = cowboy_client:response(Client2),
    {ok, Body, _} = cowboy_client:response_body(Client3),
    %% somewhere in the reply page we should have a string from label
    %% created by nitrogen page index.erl
    nomatch /= binary:match(Body, <<"some text in label for test">>),
    ok.

%% Hook for the above onrequest tests.
onrequest_hook(Req) -> Req.
