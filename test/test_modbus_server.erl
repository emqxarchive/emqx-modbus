%%%-------------------------------------------------------------------
%%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

-module(test_modbus_server).

-compile(export_all).

-behaviour(gen_server).

-define(LOGT(Format, Args),
    lager:debug("TEST_MODBUS_SERVER: " ++ Format, Args)).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-export([start_link/1, stop/0, visitors/0]).

-record(state, {listen_socket :: inet:socket(), accepted = 0, test_func2}).


start_link({Port, TestFunc, undefined}) ->
    start_link({Port, TestFunc, fun(_) -> receive A -> A end end});
start_link({Port, TestFunc, TestFunc2}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Port, TestFunc, TestFunc2}, []).

stop() ->
    gen_server:stop(?MODULE).

visitors() ->
    gen_server:call(?MODULE, get_accepted).


init({Port, TestFunc, TestFunc2}) ->
    ?LOGT("start test_modbus_server~n", []),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    accept(Listen, TestFunc),
    {ok, #state{listen_socket = Listen, test_func2 = TestFunc2}}.

handle_call(get_accepted, _From, State=#state{accepted = A}) ->
    {reply, A, State};

handle_call(Req, _From, State) ->
    ?LOGT("test_modbus_server: ignore call Req=~p~n", [Req]),
    {reply, {error, badreq}, State}.

handle_cast({accepted, _}, State=#state{listen_socket = LSockt, accepted = Acepted, test_func2 = TestFunc2}) ->
    ?LOGT("test_modbus_server, TestFun2\n", []),
    accept(LSockt, TestFunc2),
    {noreply, State#state{accepted = Acepted + 1}};

handle_cast(Msg, State) ->
    ?LOGT("test_modbus_server: ignore cast msg=~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOGT("test_modbus_server: ignore info=~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #state{listen_socket = LSocket}) ->
    ?LOGT("test_modbus_server: terminate Reason=~p~n", [Reason]),
    gen_tcp:close(LSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% To be more robust we should be using spawn_link and trapping exits
accept(LSocket, TestFunc) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, TestFunc}]).

accept_loop({Server, LSocket, TestFunc}) ->
    try
        R = gen_tcp:accept(LSocket),
        % Let the server spawn a new process and replace this loop
        % with the echo loop, to avoid blocking
        gen_server:cast(Server, {accepted, self()}),
        {ok, Socket} = R,
        ?LOGT("accept a new tcp connection, Socket=~p~n", [Socket]),
        ok = gen_tcp:controlling_process(Socket, self()),
        TestFunc(Socket)
        %gen_tcp:close(Socket)
    catch
        error:A -> A;
        throw:B -> B;
        exit:C -> C
    end,
    % do not quit this process, otherwise tcp connection will be closed
    timer:sleep(1000000).






