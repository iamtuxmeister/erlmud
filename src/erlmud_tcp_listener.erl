-module(erlmud_tcp_listener).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listen_socket, port}).

%% API Functions
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% gen_server callbacks
init([Port]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]) of
        {ok, ListenSocket} ->
            io:format("ErlMUD Listening on port ~p~n", [Port]),
            self() ! accept,
            {ok, #state{listen_socket = ListenSocket, port = Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, State = #state{listen_socket = ListenSocket}) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("New connection accepted, spawning player session~n"),
            %% Spawn a player session process
            {ok, Pid} = erlmud_player_session:start_link(Socket),
            %% Transfer ownership to the player session
            gen_tcp:controlling_process(Socket, Pid),
            %% Tell the session it can use the socket
            gen_server:cast(Pid, socket_ready),
            %% now we are waiting for more connections
            self() ! accept,
            {noreply, State};
        {error, Reason} ->
            io:format("Accpet error: ~p~n", [Reason])
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
