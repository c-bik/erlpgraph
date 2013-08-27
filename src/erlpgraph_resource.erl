-module(erlpgraph_resource).
-author('Bikram Chatterjee <razorpeak@gmail.com>').

-behaviour(cowboy_loop_handler).
 
-export([init/3]).
-export([info/3]).
-export([terminate/3]).

init({tcp, http}, Req, _Opts) ->
    display_req(Req),
    process_request(element(1,cowboy_req:path_info(Req)), self()),
    {loop, Req, undefined_state, 60000, hibernate}.
 
info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}
                                       ,{<<"content-type">>, <<"application/json">>}], Body, Req),
    {ok, Req2, State};
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

process_request([<<"all">>], RespPid) ->
    spawn(fun() ->
        RespPid ! {reply, graph:all()}
    end).

% Reply templates
% cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req),
% cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Echo, Req),
% {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
% Echo = proplists:get_value(<<"echo">>, PostVals),
% cowboy_req:reply(400, [], <<"Missing body.">>, Req)
% reply_200_json(Body, Req) ->
% 	cowboy_req:reply(200, [
%           {<<"content-encoding">>, <<"utf-8">>}
%         , {<<"content-type">>, <<"application/json">>}
%         ], Body, Req).

display_req(Req) ->
    lager:info("-------------------------------------------------------"),
    lager:info("method     ~p", [element(1,cowboy_req:method(Req))]),
    lager:info("version    ~p", [element(1,cowboy_req:version(Req))]),
    lager:info("peer       ~p", [element(1,cowboy_req:peer(Req))]),
    lager:info("host       ~p", [element(1,cowboy_req:host(Req))]),
    lager:info("host_info  ~p", [element(1,cowboy_req:host_info(Req))]),
    lager:info("port       ~p", [element(1,cowboy_req:port(Req))]),
    lager:info("path       ~p", [element(1,cowboy_req:path(Req))]),
    lager:info("path_info  ~p", [element(1,cowboy_req:path_info(Req))]),
    lager:info("qs         ~p", [element(1,cowboy_req:qs(Req))]),
    %lager:info("qs_val     ~p", [element(1,cowboy_req:qs_val(Req))]),
    lager:info("qs_vals    ~p", [element(1,cowboy_req:qs_vals(Req))]),
    %lager:info("fragment   ~p", [element(1,cowboy_req:fragment(Req))]),
    lager:info("host_url   ~p", [element(1,cowboy_req:host_url(Req))]),
    lager:info("url        ~p", [element(1,cowboy_req:url(Req))]),
    %lager:info("binding    ~p", [element(1,cowboy_req:binding(Req))]),
    lager:info("bindings   ~p", [element(1,cowboy_req:bindings(Req))]),
    lager:info("hdr(ddls)  ~p", [element(1,cowboy_req:header(<<"dderl_sess">>,Req))]),
    lager:info("hdr(host)  ~p", [element(1,cowboy_req:header(<<"host">>,Req))]),
    lager:info("headers    ~p", [element(1,cowboy_req:headers(Req))]),
    %lager:info("cookie     ~p", [element(1,cowboy_req:cookie(Req))]),
    lager:info("cookies    ~p", [element(1,cowboy_req:cookies(Req))]),
    %lager:info("meta       ~p", [element(1,cowboy_req:meta(Req))]),
    lager:info("has_body   ~p", [cowboy_req:has_body(Req)]),
    lager:info("body_len   ~p", [element(1,cowboy_req:body_length(Req))]),
    lager:info("body_qs    ~p", [element(2,cowboy_req:body_qs(Req))]),
    lager:info("body       ~p", [element(2,cowboy_req:body(Req))]),
    lager:info("-------------------------------------------------------").
