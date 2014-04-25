-module(socket_handler).

-behaviour(cowboy_websocket_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

% -export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_Type, Req0, _Opts) ->
	{Method, Req1} = cowboy_req:method(Req0),
	io:format("SOCKET HANDLER: ~p~n", [self()]),


	Ref = erlang:monitor(process, whereis(backend_socket_dispatch)),
	io:format("Monitoring backend_socket_dispatch: ~p with ref: ~p~n", [whereis(backend_socket_dispatch), Ref]),
	case Method of
		<<"GET">> ->
			{Path, Req2} = cowboy_req:path(Req1),
			case Path of
				<<"/socket">> -> {upgrade, protocol, cowboy_websocket};
				_ -> {ok, Req2, {Method}}
			end;

		_ -> {upgrade, protocol, cowboy_websocket}
	end.

handle(Req, State) ->
	io:format("~p~p~n", [Req,State]),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
		], <<"Hello Worlad!">>, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

websocket_init(_TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
	backend_socket_dispatch:add(self()),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	backend_socket_dispatch:send(Msg, self()),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
	io:format("websocket_handle: ~p~n", [_Data]),
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you dsoin'?">>),
    {reply, {text, Msg}, Req, State};

websocket_info({'DOWN', _, process, _, _}, _Req, _State) ->
	io:format("Readding pid: ~p~n",[self()]),
	backend_socket_dispatch:add(self()),
	{ok, _Req, _State};

websocket_info(Msg, Req, State) ->
	io:format("websocket_info: ~p~n", [Msg]),
    % {ok, Req, State}.
	{reply, {text, <<"resend: " ,Msg/binary >>}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	backend_socket_dispatch:remove( self()),
	io:format("websocket_terminate: ~p~n", [self()]),
    ok.