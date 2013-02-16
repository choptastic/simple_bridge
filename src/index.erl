% Nitrogen Elements Examples
% Copyright (c) 2013 Roman Shestakov (romanshestakov@yahoo.co.uk)
% See MIT-LICENSE for licensing information.

-module(index).

%% -include_lib("nitrogen_elements/include/nitrogen_elements.hrl").
-include_lib ("nitrogen_core/include/wf.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% main() ->
%%     %% #template { file="/Users/romanshestakov/Development/nitrogen_elements_examples/deps/simple_bridge/templates/onecolumn.html" }.
%%     %% #template { file= ?config(data_dir, Config) ++ "onecolumn.html" }.
%%     #template { file="/templates/onecolumn.html" }.
main() ->
    #template { file=filename:join([web_common:templates(), "onecolumn.html"])
}.

title() -> "Nitrogen Elements Examples".
headline() -> "Nitrogen Elements Examples".

body() -> [
	   #label{text = "label test"}
].

