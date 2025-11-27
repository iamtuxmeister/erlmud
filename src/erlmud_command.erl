-module(erlmud_command).

-export([process/2]).

%% process(PlayerPid, CommandString) -> ok
%% Routes commands to appropriate handlers

process(PlayerPid, CommandString) ->
    Tokens = parse_input(CommandString),
    route_command(PlayerPid, Tokens).

%% Internal functions
parse_input(Binary) when is_binary(Binary) ->
    parse_input(binary_to_list(Binary));
parse_input(String) ->
    Line = string:trim(String),
    string:tokens(
        string:to_lower(Line), " ").

route_command(_PlayerPid, []) ->
    ok;
route_command(PlayerPid, ["quit"]) ->
    handle_quit(PlayerPid);
route_command(PlayerPid, ["look"]) ->
    handle_look(PlayerPid);
route_command(PlayerPid, Tokens) ->
    handle_unknown(PlayerPid, Tokens).

%% Command handlers

handle_quit(PlayerPid) ->
    %% Send a message to the player session to disconnect
    PlayerPid ! {system_message, <<"Goodbye!\r\n">>},
    PlayerPid ! disconnect,
    ok.

handle_look(PlayerPid) ->
    %% For now, we don't have rooms yet, so send a placeholder
    %% Later: RoomPid = get_player_room(PlayerPid),
    %%        gen_server:call(RoomPid, {look, PlayerPid})
    PlayerPid ! {system_message, <<"You are standing in a void. Nothing exists yet.\r\n">>},
    ok.

handle_unknown(PlayerPid, Tokens) ->
    Message = io_lib:format("Unknown command: ~s\r\n", [string:join(Tokens, " ")]),
    PlayerPid ! {system_message, list_to_binary(Message)},
    ok.
