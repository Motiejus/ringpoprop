-module(tchannel).

-define(TCHANNEL_LIB_VERSION, <<"0.1">>).  % version of this tchannel library
-define(TCHANNEL_VERSION, 2).

-define(DEFAULT_TCP_CONNECT_TIMEOUT, 500).
-define(DEFAULT_INIT_TIMEOUT, 500).

-type packet_type() ::
    init_req          | % First message on every connection must be init
    init_res          | % Remote response to init req
    call_req          | % RPC method request
    call_res          | % RPC method response
    call_req_continue | % RPC request continuation fragment
    call_res_continue | % RPC response continuation fragment
    cancel            | % Cancel an outstanding call req / forward req (no body)
    claim             | % Claim / cancel a redundant request
    ping_req          | % Protocol level ping req (no body)
    ping_res          | % Ping res (no body)
    error.              % Protocol level error

-type option() ::
    {tcp_connect_timeout, timeout()} |
    {init_timeout, timeout()}.

-type error_reason() ::
    {option, any()} |
    closed |
    inet:posix().

-type packet_id() :: 0..16#fffffffe.

-record(state, {
          sock :: gen_tcp:socket(),
          options :: [option()],
          headers :: [{binary(), binary()}],
          version :: pos_integer()  % tchannel version reported by remote
}).
-opaque state() :: #state{}.
-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([connect/2, connect/3, headers/1, header/2, close/1]).

connect(Address, Port) ->
    connect(Address, Port, []).

%% @doc Connect to a tchannel endpoint.
%%
%% There are two kinds of time outs in the Options:
%% * tcp_connect_timeout :: time limit to establish TCP session.
%% * init_timeout :: time limit to do a phase per init. Actual init time
%%   will be a few of times longer. TODO: use this value.
-spec connect(Address, Port, Options) -> {ok, State} | {error, Reason} when
      Address :: inet:ip_address() | inet:hostname(),
      Port :: inet:port_number(),
      Options :: [option()],
      State :: state(),
      Reason :: error_reason().
connect(Address, Port, Options) ->
    case check_options(Options) of
        ok ->
            connect_1(Address, Port, merge_options(Options));
        Error ->
            Error
    end.

-spec headers(State) -> [{HeaderKey, HeaderVal}] when
      State :: state(),
      HeaderKey :: binary(),
      HeaderVal :: binary().
headers(#state{headers=Headers}) ->
    Headers.

-spec header(State, HeaderKey) -> HeaderVal when
      State :: state(),
      HeaderKey :: binary(),
      HeaderVal :: undefined | binary().
header(#state{headers=Headers}, HeaderKey) ->
    proplists:get_value(HeaderKey, Headers).

-spec close(State) -> ok when
      State :: state().
close(#state{sock=Socket}) ->
    gen_tcp:close(Socket).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_options([]) ->
    ok;
check_options([{tcp_connect_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([{init_timeout, T}|Options]) when is_integer(T) ->
    check_options(Options);
check_options([Opt|_]) ->
    {error, {options, Opt}}.

%% @doc Merges user-defined and default options
-spec merge_options([option()]) -> [option()].
merge_options(Options) ->
    F = fun(K, Default) -> {K, proplists:get_value(K, Options, Default)} end,
    [
     F(tcp_connect_timeout, ?DEFAULT_TCP_CONNECT_TIMEOUT),
     F(init_timeout, ?DEFAULT_INIT_TIMEOUT)
    ].

connect_1(Address, Port, Options) ->
    Timeout = proplists:get_value(tcp_connect_timeout, Options),
    case gen_tcp:connect(Address, Port, [binary, {active, false}], Timeout) of
        {ok, Sock} ->
            State = #state{sock=Sock, options=Options},
            init_req(State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec init_req(State) -> {ok, State} | {error, Reason} when
      State :: state(),
      Reason:: error_reason().
init_req(#state{sock=Sock}=State) ->
    Packet = construct_init_req(),
    case gen_tcp:send(Sock, Packet) of
        ok ->
            init_res(State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec init_res(State) -> {ok, State} | {error, Reason} when
      State :: state(),
      Reason:: error_reason().
init_res(#state{sock=Sock, options=Options}=State) ->
    Timeout = proplists:get_value(init_timeout, Options),
    case gen_tcp:recv(Sock, 4, Timeout) of
        {ok, <<Version:16, NH:16>>} ->
            State2 = State#state{version=Version},
            receive_headers(State2, NH);
        {error, Reason} ->
            {error, Reason}
    end.

-spec receive_headers(State, NH) -> {ok, State} | {error, Reason} when
      State :: state(),
      NH :: pos_integer(),
      Reason :: error_reason().
receive_headers(State, NH) ->
    receive_headers(State, NH, []).

receive_headers(State, 0, Acc) ->
    {ok, State#state{headers=Acc}};
receive_headers(#state{sock=Sock}=State, NH, Acc) ->
    Timeout = proplists:get_value(init_timeout, State#state.options),
    case receive_header_value(Sock, Timeout) of
        {ok, Key} ->
            case receive_header_value(Sock, Timeout) of
                {ok, Value} ->
                    Acc2 = [{Key, Value} | Acc],
                    receive_headers(State, NH+1, Acc2);
                {error, Reason2} ->
                    {error, Reason2}
            end;
        {error, Reason1} ->
            {error, Reason1}
    end.

%% @doc Receive size-2 key and value from a socket.
-spec receive_header_value(Sock, Timeout) -> {ok, Value} | {error, Reason} when
      Sock :: gen_tcp:socket(),
      Timeout :: timeout(),
      Value :: binary(),
      Reason :: error_reason().
receive_header_value(Sock, Timeout) ->
    case gen_tcp:recv(Sock, 2, Timeout) of
        {ok, <<HeaderLen:16>>} ->
            gen_tcp:recv(Sock, HeaderLen, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Construct packet for init_req
-spec construct_init_req() -> Packet when Packet :: iodata().
construct_init_req() ->
    OTP = list_to_binary(erlang:system_info(otp_release)),
    Headers = [
               {<<"host_port">>, <<"0.0.0.0:0">>},
               {<<"process_name">>, <<"ringpoprop">>},
               {<<"tchannel_language">>, <<"erlang">>},
               {<<"tchannel_language_version">>, OTP},
               {<<"tchannel_version">>, ?TCHANNEL_LIB_VERSION}
              ],
    HeaderPayload =
    [
     <<(length(Headers)):16>>,
     [<<
        (iolist_size(K)):16, K/binary,
        (iolist_size(V)):16, V/binary
      >> || {K, V} <- Headers
     ]
    ],

    Payload = [
               <<?TCHANNEL_VERSION:16>>,
               HeaderPayload
              ],
    construct_packet(init_req, 1, Payload).


%% @doc Construct a tchannel packet.
%%
%% Size must be <= 2^16, otherwise it will silently trim the size. Streaming
%% should be done at higher level.
-spec construct_packet(Type, Id, Payload) -> Packet when
      Type :: packet_type(),
      Id :: packet_id(),
      Payload :: iodata(),
      Packet :: iodata().
construct_packet(Type, Id, Payload) ->
    Size = iolist_size(Payload) + 16,
    TypeNum = type_num(Type),
    [<<Size:16,
      TypeNum:8,
      0:8,  % reserved byte
      Id:32,
      0:64>>, % 8 reserved bytes
     Payload].


%% @doc Numeric tchannel packet type.
-spec type_num(packet_type()) -> 0..16#ff.
type_num(init_req) ->               16#01.
%type_num(init_res) ->               16#02;
%type_num(call_req) ->               16#03;
%type_num(call_res) ->               16#04;
%type_num(call_req_continue) ->      16#13;
%type_num(call_res_continue) ->      16#14;
%type_num(cancel) ->                 16#c0;
%type_num(claim) ->                  16#c1;
%type_num(ping_req) ->               16#d0;
%type_num(ping_res) ->               16#d1;
%type_num(error) ->                  16#ff.
