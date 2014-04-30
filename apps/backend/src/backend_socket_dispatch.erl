-module(backend_socket_dispatch).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
        add/2,
        send/3,
        remove/2,
        whereis/0
]).

-record(state, {open_sockets, open_sessions}).

remove(SessionId, Pid) ->
    gen_server:cast(?MODULE, {remove, SessionId, Pid}).

add(SessionId, Pid) ->
    gen_server:cast(?MODULE, {add, SessionId, Pid}).

send(SessionId, Msg, Pid) ->
    gen_server:cast(?MODULE, {send, SessionId, Msg, Pid}).

whereis() ->
    whereis(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("Stopping~n"),
    gen_server:cast(?MODULE, shutdown).

init(_) ->
    process_flag(trap_exit, true),
	  State = #state{open_sessions = orddict:new(), open_sockets = orddict:new()},
    {ok, State}.

handle_call(Message, From, State) ->
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

handle_cast({add, SessionId, Pid}, State) ->

    PidList = case  orddict:find(SessionId, State#state.open_sessions) of
                  {ok, List} -> case lists:member(Pid, List) of
                                    true -> List;
                                    false -> [Pid | List]
                                end;
                  error -> [Pid]
              end,

	  NewState = State#state{
        open_sockets = orddict:store(Pid, Pid, State#state.open_sockets),
        open_sessions = orddict:store(SessionId, PidList, State#state.open_sessions)
    },

	  {noreply, NewState};

handle_cast({remove, SessionId, Pid}, State) ->

    Sessions = case orddict:find(SessionId, State#state.open_sessions) of
                   {ok, List} -> List;
                   error -> []
               end,

    NewPids = lists:delete(Pid, Sessions),
    NewState = State#state{open_sockets = State#state.open_sockets, open_sessions = orddict:store(SessionId, NewPids, State#state.open_sessions)},

	  {noreply, NewState};

handle_cast({send, SessionId, Msg, SelfPid}, State) ->
    {ok, PidList} = orddict:find(SessionId, State#state.open_sessions),
    lists:foreach(fun(Pid) when Pid =/= SelfPid -> Pid ! Msg; (_) -> false end , PidList),
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
