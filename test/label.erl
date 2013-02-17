% Copyright (c) 2013 Roman Shestakov (romanshestakov@yahoo.co.uk)
% See MIT-LICENSE for licensing information.

-module(label).
-include_lib ("nitrogen_core/include/wf.hrl").
-compile(export_all).
-define(APP, simple_bridge).

main() ->
    #template{file=filename:join([templates(), "test/cowboy_request_bridge_SUITE_data/onecolumn.html"])}.

templates() ->
    filename:join(lists:reverse(tl(lists:reverse(filename:split(code:priv_dir(?APP)))))).

%% title() -> "Simple_Bridge Test".
%% headline() -> "Simple_Bridge Test".

body() ->
    [
	#label{text = "label test"}
    ].
