-module(erlpgraph_app).
-author('Bikram Chatterjee <razorpeak@gmail.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
            {"/", erlpgraph, []},
            {"/app/[...]", erlpgraph_resource, []},
            {"/[...]", cowboy_static, [
                {directory, {priv_dir, erlpgraph, []}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
		]}
	]),

    {ok, Ip}         = application:get_env(erlpgraph, interface),
    {ok, Port}       = application:get_env(erlpgraph, port),

    lager:info("starting erlpgraph at http://~s:~p",
                [if is_list(Ip) -> Ip; true -> lists:flatten(io_lib:format("~p",[Ip])) end, Port]),

    {ok, Interface} = inet:getaddr(Ip, inet),
    {ok, _} = cowboy:start_http(http, 100, [{ip, Interface},
		                                    {port, Port}],
                                           [{env, [{dispatch, Dispatch}]}]),
    erlpgraph_sup:start_link().

stop(_State) ->
    ok.
