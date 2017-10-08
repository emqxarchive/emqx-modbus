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

-module(emqx_modbus_device_mode1).

-behaviour(gen_server).

-include("emqx_modbus.hrl").

%% API Exports
-export([connect/3, disconnect/1, send_request/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { server_addr :: term(),
          socket      :: inet:socket(),
          error_count :: integer(),
          device_name :: binary(),
          parser      :: term(),
          pending_req :: undefined | term(),
          msgid       :: integer(),
          handler     :: atom()
        }).

-define(CALL_TIMEOUT, 60000).

-define(SOCK_TIMEOUT, 5000).

-define(RECV_TIMEOUT, 10000).

-define(SEND_TIMEOUT, 5000).

-define(ERROR_COUNT_MAX, 5).

%% Modbus Socket Options
-define(SOCKOPTS, [
        binary,
        {active, once},
        {packet, 0},
        {send_timeout, ?SEND_TIMEOUT},
        {reuseaddr, true},
        {nodelay, true}]).

%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------

connect(Host, Port, DeviceName) ->
    DeviceNameBinary = device_name(DeviceName),
    gen_server:start_link({via, ?MODBUS_VIA_MODULE, DeviceNameBinary}, ?MODULE, [Host, Port, DeviceNameBinary], []).

disconnect(Device) ->
    cast(Device, stop).

send_request(Device, Req) ->
    cast(Device, {send_request_to_device, Req}).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

%% call(Device, Request) ->
%%    gen_server:call({via, ?MODBUS_VIA_MODULE, device_name(Device)}, Request).

cast(Device, Request) ->
    gen_server:cast({via, ?MODBUS_VIA_MODULE, device_name(Device)}, Request).

init([Host, Port, DeviceName]) ->
    true = is_binary(DeviceName),
    ?LOG(debug, "start device ~p", [DeviceName]),
    self() ! reconnect,  % DO not call gen_tcp:connect() here, since it may be stuck.
    Handler = application:get_env(?APP, handler, emqx_modbus_user_data),
    {ok, #state{socket      = undefined,
                error_count = 0,
                parser      = emqx_modbus_frame:init(),
                device_name = DeviceName,
                server_addr = {Host, Port},
                msgid       = 0,
                handler     = Handler}}.

handle_call(Req, _From, State) ->
    ?LOG(error, "emqx_modbus_device, ignore unknown call=~p~n", [Req]),
    {reply, {error, badreq}, State, hibernate}.

handle_cast({send_request_to_device, Req=#modbus_req{}}, State=#state{socket = undefined}) ->
    {noreply, State#state{pending_req = Req}};

handle_cast({send_request_to_device, Req=#modbus_req{}}, State=#state{}) ->
    NewState = process_request(Req, State),
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal,State};

handle_cast(Msg, State) ->
    ?LOG(error, "emqx_modbus_device, ignore unknow msg=~p", [Msg]),
    {noreply, State, hibernate}.

handle_info(reconnect, State=#state{device_name = DevinceName,
                                    pending_req = PendingReq,
                                    server_addr = {Host, Port}}) ->
    case connect_tcp(Host, Port) of
        {ok, Sock} ->
            ?LOG(info, "success connected ~p ~p:~p", [DevinceName, Host, Port]),
            {noreply, process_request(PendingReq,
                                      State#state{socket = Sock,
                                                  error_count = 0,
                                                  pending_req = undefined})};
        {error, _Error} ->
            ?LOG(error, "fail to connect ~p ~p:~p", [DevinceName, Host, Port]),
            {noreply, reconnect_tcp(false, State), hibernate}
    end;

handle_info({tcp, Sock, Data}, State=#state{msgid = MsgId, parser = Parser,device_name = DeviceName, handler = Handler}) ->
    inet:setopts(Sock,[{active,once}]),
    ?LOG(debug, "receive tcp data ~p", [Data]),
    case emqx_modbus_frame:parse(Data, Parser) of
        {more_data, Parser1} ->
            {noreply, State#state{parser = Parser1}};
        {ok, #modbus_frame{header = #mbap_header{unit_id = UnitId}, funcode = Funcode, payload = ModbusData}, Parser2} ->
            Rsp = #modbus_rsp{msgid = MsgId, device_name = DeviceName, uid = UnitId, funcode = Funcode, data = ModbusData},
            MqttPayload = Handler:handle_ul_data(Rsp),
            emqx_modbus_control:send_response(MqttPayload),
            {noreply, State#state{parser = Parser2}};
        {error, Parser3} ->
            {noreply, State#state{parser = Parser3}}
    end;

handle_info({tcp_error, Sock, Reason}, State = #state{}) ->
    ?LOG(warning, "tcp error, Sock=~p, Reason=~p~n", [Sock, Reason]),
    {stop, {shutdown, {tcp_error, Reason}}, State};

handle_info({tcp_closed, Sock}, State) ->
    ?LOG(debug, "tcp connection has been closed, Sock=~p", [Sock]),
    {noreply, reconnect_tcp(true, State)};

handle_info({suback, _, _}, State) ->
    {noreply, State};

handle_info({subscribe, _}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(error, "emqx_modbus_device, ignore unknow info=~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

connect_tcp(Host, Port) ->
    gen_tcp:connect(Host, Port, ?SOCKOPTS, ?SOCK_TIMEOUT).

reconnect_tcp(false, State) ->
    erlang:send_after(10, self(), reconnect),
    reconnect_tcp(State);
reconnect_tcp(true, State) ->
    self() ! reconnect,
    reconnect_tcp(State).

reconnect_tcp(State=#state{socket = Sock}) ->
    Sock =/= undefined andalso gen_tcp:close(Sock),
    State#state{socket = undefined}.

process_request(undefined, State) ->
    State;
process_request(Req=#modbus_req{msgid = MsgId, uid = UnitId, funcode = FunCode, data = Data},
                State=#state{socket = Sock, parser = Parser}) ->
    ?LOG(debug, "handle_pending_req ~p", [Req]),
    {ModBusData, NewParser} = emqx_modbus_frame:serialize(UnitId, FunCode, Data, Parser),
    case send_data(ModBusData, Sock) of
        ok ->
            State#state{msgid = MsgId, parser = NewParser, error_count = 0};
        {error, timeout} ->
            % ignore tcp send timeout
            process_tcp_timeout(State#state{msgid = MsgId, parser = NewParser});
        {error, _Reason} ->
            reconnect_tcp(true, State)
    end.

process_tcp_timeout(State=#state{error_count = Count}) ->
    NewCount = Count + 1,
    case NewCount >= ?ERROR_COUNT_MAX of
        true -> reconnect_tcp(true, State);
        false -> State#state{error_count = NewCount}
    end.

send_data(ModBusData, Sock) ->
    ?LOG(debug, "send tcp data ModBusData=~p, Sock=~p", [ModBusData, Sock]),
    gen_tcp:send(Sock, ModBusData).

device_name(DeviceName) when is_atom(DeviceName) ->
    atom_to_binary(DeviceName, utf8);
device_name(DeviceName) when is_list(DeviceName) ->
    list_to_binary(DeviceName);
device_name(DeviceName) when is_binary(DeviceName) ->
    DeviceName.

