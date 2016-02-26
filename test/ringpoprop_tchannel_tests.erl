-module(ringpoprop_tchannel_tests).

-include_lib("eunit/include/eunit.hrl").

%% @doc Starts tchannel json echo service. Tells where it's listening on.
-spec start_echo_server() -> {ok, Handler, HostPort} when
      Handler :: port(),
      HostPort :: string().
start_echo_server() ->
    {ok, Dir} = file:get_cwd(),
    Python = filename:join([Dir, "_build", "venv", "bin", "python"]),
    Server = filename:join([Dir, "test", "tchannel_echo.py"]),

    Port = open_port({spawn_executable, Python}, [{args, [Server]}]),
    Data = receive
        {Port, {data, Data}} ->
            string:strip(Data)
    end,
    {Port, Data}.


basic_exported_test() ->
    {Port, HostPort} = start_echo_server(),
    io:format(user, "HostPort: ~s~n", [HostPort]).
