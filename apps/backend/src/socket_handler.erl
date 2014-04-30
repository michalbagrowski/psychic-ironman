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

-record(state, {session_id}).

init(_Type, Req0, _Opts) ->
	{Method, Req1} = cowboy_req:method(Req0),
	io:format("init socket handler: ~p~n", [self()]),

	case Method of
		<<"GET">> ->
			{Path, Req2} = cowboy_req:path(Req1),
			case Path of
				<<"/socket">> -> {upgrade, protocol, cowboy_websocket};
				_ -> {ok, Req2, {Method}}
			end;

		_ -> {upgrade, protocol, cowboy_websocket}
	end.

monitor()->
	Ref = erlang:monitor(process, backend_socket_dispatch:whereis()),
	io:format("Monitoring backend_socket_dispatch: ~p with ref: ~p~n", [whereis(backend_socket_dispatch), Ref]).

handle(Req, State) ->
	  io:format("~p~p~n", [Req, State]),
	  {ok, Req2} = cowboy_req:reply(200, [
		    {<<"content-type">>, <<"text/plain">>}
		    ], <<"Hello Worlad!">>, Req),
	  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	  ok.

websocket_init(_TransportName, Req, _Opts) ->
	  io:format("get new websocket connection ~n"),
    monitor(),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->

    SessionId = Msg,
    backend_socket_dispatch:add(SessionId, self()),

    NewState = #state{session_id = SessionId},
    backend_socket_dispatch:send(NewState#state.session_id, {text, Msg}, self()),
    {ok, Req, NewState};

websocket_handle({binary, Msg}, Req, State) ->
	  backend_socket_dispatch:send(State#state.session_id, {binary, Msg}, self()),
    {ok, Req, State};


websocket_handle(_Data, Req, State) ->
    io:format("websocket_handle: ~p~n", [_Data]),
    {ok, Req, State}.

websocket_info({'DOWN', _, process, _, _}, _Req, State) ->
    monitor(),
	  io:format("Readding pid: ~p~n",[self()]),
	  backend_socket_dispatch:add(State#state.session_id, self()),
	  {ok, _Req, State};

websocket_info({text, Msg}, Req, State) ->
    io:format("text message: ~p~n", [Msg]),
    {reply, {text, << Msg/binary >>}, Req, State};

websocket_info({binary, Msg}, Req, State) ->
    io:format("binary message: ~p~n", [Msg]),
    {reply, {binary, Msg}, Req, State};

websocket_info(Msg, Req, State) ->
	  io:format("UNKNOWN MESSAGE: ~p~n", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	  backend_socket_dispatch:remove( self()),
	  io:format("websocket_terminate: ~p~n", [self()]),
    ok.