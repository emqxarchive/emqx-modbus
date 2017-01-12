%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(test_modbus_server).

-compile(export_all).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/1, stop/0, set_case/1]).

-record(state, {
    listen_socket :: inet:socket(),
    socket      :: inet:socket(),
    case_num    :: integer()
}).


start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:stop(?MODULE).

set_case(CaseNumber) ->
    io:format("API set server name=~p, case=~p~n", [?MODULE, CaseNumber]),
    gen_server:cast(?MODULE, {set_case, CaseNumber, self()}).


init([Port]) ->
    process_flag(trap_exit, true),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, true}]),
    {ok, #state{listen_socket = Listen}}.



handle_call(stop, _From, State=#state{socket = Sock}) ->
    gen_tcp:close(Sock),
    {stop, normal, stopped, State};

handle_call(Req, _From, State) ->
    io:format("test_modbus_server: ignore call Req=~p~n", [Req]),
    {reply, {error, badreq}, State}.

handle_cast({set_case, CaseNumber, From}, State=#state{listen_socket = Listen}) ->
    io:format("server handle set case=~p~n", [CaseNumber]),
    From ! go,
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("server accept socket=~p~n", [Socket]),
    gen_tcp:recv(Socket, 0),
    {noreply, State#state{case_num = CaseNumber}};

handle_cast(start_accept, State=#state{listen_socket = Listen}) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_server:cast(self(), start_recv),
    {noreply, State#state{socket = Socket}};

handle_cast(start_recv, State=#state{socket = Socket}) ->
    gen_tcp:recv(Socket, 0),
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("test_modbus_server: ignore cast msg=~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State=#state{case_num = CaseNumber}) ->
    Tid = validate_request(Bin, CaseNumber),
    send_to_edge(Socket, CaseNumber, Tid),
    {noreply, State};

handle_info(Info, State) ->
    io:format("test_modbus_server: ignore info=~p~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("test_modbus_server: terminate Reason=~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_to_edge(Socket, CaseNumber, Tid) ->
    Body = get_respond_data(CaseNumber),
    Len = size(Body) + 1,
    Data = case CaseNumber of
        4 -> <<Tid:16, 42:16, Len:16, 0:8, Body/binary>>;
        5 -> <<Tid:16, 42:0, Len:16, 0:8>>;
        _ -> <<Tid:16, 0:16, Len:16, 0:8, Body/binary>>
    end,
    io:format("send to edge ~p~n", [Data]),
    gen_tcp:send(Socket, Data).

validate_request(Bin, CaseNumber) ->
    ExpectedRaw = expected_tcp_data(CaseNumber),
    <<Tid:16, 0:16, Rest/binary>> = Bin,
    Len = size(ExpectedRaw)+1,
    Expected = <<Len:16, 0:8, ExpectedRaw/binary>>,
    io:format("case ~p expected ~p~n", [CaseNumber, Expected]),
    io:format("  against received ~p~n", [Rest]),
    Expected = Rest,
    Tid.



expected_tcp_data(CaseNumber) ->
    case CaseNumber of
        1 -> <<7, 8, 9, 10>>;
        4 -> <<7, 8, 9, 10>>;
        5 -> <<7, 8, 9, 10>>;
        6 -> <<7, 8, 9, 10>>
    end.

get_respond_data(CaseNumber) ->
    case CaseNumber of
        1 -> <<7, 20, 30, 40>>;
        4 -> <<32>>;
        5 -> <<>>;
        6 -> <<>>
    end.


