-module(erlmud_player_session).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket, player_name}).

%% API Functions
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%% gen_server callbacks
init([Socket]) ->
    %% Don't take ownership, just start stuff
    {ok, #state{socket = Socket, player_name = undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(socket_ready, State = #state{socket = Socket}) ->
    %% Now we own the socket, start receiving
    inet:setopts(Socket, [{active, once}]),
    gen_tcp:send(Socket, <<"What is your name?\r\n">>),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data},
            State = #state{socket = Socket, player_name = undefined}) ->
    Name = string:trim(binary_to_list(Data)),
    gen_tcp:send(Socket,
                 list_to_binary(io_lib:format("Hello, ~s! Welcome to ErlMUD!\r\n", [Name]))),
    gen_tcp:send(Socket, <<"> ">>),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{player_name = Name}};
handle_info({tcp, Socket, Data}, State = #state{socket = Socket, player_name = Name})
    when Name =/= undefined ->
    erlmud_command:process(self(), Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({system_message, Message}, State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, Message),
    gen_tcp:send(Socket, <<"> ">>),
    {noreply, State};
handle_info(disconnect, State) ->
    {stop, normal, State};
handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
    io:format("Player disconnected~n"),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
