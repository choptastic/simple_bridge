
-module(simple_bridge_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
	all/0,
	groups/0,
	init_per_group/2,
	end_per_group/2
]).

-export([
	peer_ip/1,
	protocol/1,
	path/1,
	uri/1,
	request_method_get/1,
	request_method_post/1,
	request_body/1,
	query_params/1,
	post_params/1,
	deep_post_params1/1,
	static/1,
	deep_static/1,
	deeper_static/1
]).

all() -> [{group, main}].

groups() ->
	[{main, 
		[parallel, {repeat, 1}],
		[
			peer_ip, request_method_get, request_method_post, request_body,
			protocol, path, query_params,
			post_params, deep_post_params1,
			static, deep_static, deeper_static
		]
	}].

init_per_group(main, Config) ->
	inets:start(),
	application:start(simple_bridge),
	Config.

end_per_group(main, Config) ->
	inets:stop(),
	application:stop(simple_bridge),
	Config.


peer_ip(_) ->
	"{127,0,0,1}" = request("peer_ip").

protocol(_) ->
	"http" = request("protocol").

path(_) ->
	"\"/path\"" = request("path").

uri(_) ->
	"http://127.0.0.1:8000/uri" = request("uri").

request_method_get(_) ->
	"'GET'" = request("request_method_get").

request_method_post(_) ->
	"'POST'" = post("request_method_post", "").

request_body(_) ->
	"<<\"my body\">>" = post("request_body", "my body").
	
query_params(_) ->
	"[{<<\"a\">>,<<\"1\">>},{<<\"b\">>,<<\"2\">>}]" = request("query_params?a=1&b=2").

post_params(_) ->
	"[{<<\"a\">>,<<\"1\">>},{<<\"b\">>,<<\"2\">>}]" = post("post_params", "a=1&b=2").

deep_post_params1(_) ->
	"y" = post("deep_post_params", "a[0]=x&a[1]=y").
	
static(_) ->
	"1234\n" = get_static("static/test_static.txt").

deep_static(_) ->
	"1\n" = get_static("static/deep/depth1.txt").

deeper_static(_) ->
	"2\n" = get_static("static/deep/deeper/depth2.txt").

get_static(File) ->
	URL = "http://127.0.0.1:8000/" ++ File,
	{ok, {_, _, Val}} = httpc:request(URL),
	Val.


request(Path) ->
	URL = "http://127.0.0.1:8000/" ++ Path,
	{ok, {_, _, Val}} = httpc:request(URL),
	Val.

post(Path, Body) ->
	URL = "http://127.0.0.1:8000/" ++ Path,
	%% The content-type is particularly critical for mochiweb. Mochiweb won't
	%% parse body values unless its content-type is set to
	%% application/x-www-form-urlencoded
	{ok, {_, _, Val}} = httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Body}, [], []),
	Val.
