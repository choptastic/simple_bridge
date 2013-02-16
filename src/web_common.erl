% Nitrogen Elements Examples
% Copyright (c) 2013 Roman Shestakov (romanshestakov@yahoo.co.uk)
% See MIT-LICENSE for licensing information.

-module(web_common).
-compile(export_all).
-export([templates/0]).

-include_lib ("nitrogen_core/include/wf.hrl").

%% docroot() ->
%%     code:priv_dir(simple_bridge) ++ "/static".

templates() ->
    code:priv_dir(simple_bridge) ++ "/templates".

%% header(Selected) ->
%%     wf:wire(Selected, #add_class { class=selected }),
%%     #panel { class=menu, body=[
%%         #link { id=index, url='/', text="INDEX" }
%%     ]}.

%% footer() ->
%%     #panel { class=credits, body=[
%%         "
%%         "
%%     ]}.

%% assert_path( Str ) when is_list( Str ) ->
%%     assert_path( #template {
%%        file=filename:join([templates(), Str])
%% });

%% assert_path( Elem=#template {} ) ->
%%        Elem.
