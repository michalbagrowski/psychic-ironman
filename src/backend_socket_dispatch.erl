-module(backend_socket_dispatch).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add/1]).

-record(sockets, {open_sockets}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).




stop() ->
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).

init(_) ->
    process_flag(trap_exit, true),
	State = #sockets{open_sockets=orddict:new()},
    {ok, State}.

handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

handle_cast({add,Pid}, State) ->
	Open = orddict:store("test", Pid, State#sockets.open_sockets),
	NewState = State#sockets{open_sockets = Open},
	{noreply, NewState};

handle_cast({send,Msg}, State) ->
	orddict:map(fun(_, Pid) -> Pid ! Msg end,  State#sockets.open_sockets);

handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};

handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

handle_info(_Message, _Server) ->
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

terminate(_Reason, _Server) ->
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.

add(Pid) ->
	% orddict:store("test", Pid, #sockets.open_sockets).
	gen_server:cast(?MODULE, {add, Pid}).

send(Msg) ->
	gen_server:cast(?MODULE, {send, Msg}).

