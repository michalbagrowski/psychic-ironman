-module(backend_socket_dispatch).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([add/1, send/2, remove/1]).

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
	NewState = State#sockets{open_sockets = orddict:store(Pid, Pid, State#sockets.open_sockets)},
	{noreply, NewState};

handle_cast({remove,Pid}, State) ->
	NewState = State#sockets{open_sockets = orddict:erase(Pid, State#sockets.open_sockets)},
	{noreply, NewState};


handle_cast({send,Msg, _SelfPid}, State) ->
	orddict:map(fun(_, Pid) when Pid =/= _SelfPid  ->  Pid ! Msg; (_, _) -> false end,  State#sockets.open_sockets),
	{noreply, State};

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

remove(Pid) ->
	gen_server:cast(?MODULE, {remove, Pid}).

add(Pid) ->
	gen_server:cast(?MODULE, {add, Pid}).

send(Msg, Pid) ->
	gen_server:cast(?MODULE, {send, Msg, Pid}).

whereis() ->
    whereis(?MODULE).