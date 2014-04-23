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
	error_logger:info_msg("~p~p~n", [Req,State]),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
		], <<"Hello Worlad!">>, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.



% init({tcp, http}, Req, Opts) ->
    % {upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, _Opts) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.