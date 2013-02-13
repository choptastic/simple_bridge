-module(cowboy_request_bridge_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("cowboy/include/http.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Req = #http_req{socket = undefined,
		    transport = cowboy_tcp_transport,
		    connection = keepalive,
		    pid = undefined,
		    method = 'GET',
		    version = {1,1},
		    peer = undefined,
		    host = [<<"localhost">>],
		    host_info = undefined,
		    raw_host = <<"localhost">>,
		    port = 8000,
		    path = [],
		    path_info = undefined,
		    raw_path = <<"/">>,
		    qs_vals = undefined,
		    raw_qs = <<>>,
		    bindings = [],
		    headers =
			[{'Connection',<<"keep-alive">>},
			 {'Accept-Encoding',<<"gzip, deflate">>},
			 {'Accept-Language',<<"en-us">>},
			 {'Accept',<<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>},
			 {'User-Agent',<<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_5) AppleWebKit/536.26.17 (KHTML, like Gecko) Version/6.0.2 Safari/536.26.17">>},
			 {'Host',<<"localhost">>}],
		    p_headers = [{'Connection',[<<"keep-alive">>]}],
		    cookies = undefined,
		    meta = [],
		    body_state = waiting,
		    buffer = <<>>,
		    resp_state = waiting,
		    resp_headers = [],
		    resp_body = <<>>,
		    onresponse = undefined,
		    urldecode = undefined},

    ReqBridge = simple_bridge:make_request(cowboy_request_bridge, {Req, "./priv"}),
    %% Method = ReqBridge:request_method(),
    %% 'GET' = Method,
    %% ct:log("-> get_method, ~p, ~p", [Method, ReqBridge]),
    [{reg, Req}, {req_bridge, ReqBridge} | Config].

end_per_testcase(_TestCase, Config) ->
    ok.

all() ->
    [
     method
    ].

method(Config) ->
    ReqBridge = ?config(req_bridge, Config),
    Method = ReqBridge:request_method(),
    %% 'GET' = Method,
    ct:log("-> get_bridge, ~p", [ReqBridge]),
    ok.
