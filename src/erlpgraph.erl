-module(erlpgraph).

-export([start/0,
        init/3,
        handle/2,
        terminate/3,
        stop/0]).

start() ->
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, [{lager_console_backend, info},
                                               {lager_file_backend, [{file, "log/error.log"},
                                                                     {level, error},
                                                                     {size, 10485760},
                                                                     {date, "$D0"},
                                                                     {count, 5}]},
                                               {lager_file_backend, [{file, "log/console.log"},
                                                                     {level, info},
                                                                     {size, 10485760},
                                                                     {date, "$D0"},
                                                                     {count, 5}]}]),
    ok = application:set_env(lager, error_logger_redirect, false),
    ok = application:start(lager),
    ok = application:load(sasl),
    ok = application:set_env(sasl, sasl_error_logger, false),
    ok = application:start(sasl),
    ok = application:start(os_mon),
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

% cowboy filler
% to serve the first index.html file
init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	Html = get_html(),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

get_html() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} -> "priv";
        PDir -> PDir
    end,
	Filename = filename:join([PrivDir, "index.html"]),
	{ok, Binary} = file:read_file(Filename),
	Binary.
