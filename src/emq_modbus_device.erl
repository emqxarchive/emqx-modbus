%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% modbus client which connects device.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emq_modbus_device).

-include("emodbus.hrl").

%% API Exports
-export([connect/3, disconnect/1, send_request/2]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    server_addr     :: term(),
        socket      :: inet:socket(),
    device_name     :: binary(),
        tid = 1     :: 1..16#ff
 }).

-define(CALL_TIMEOUT, 60000).

-define(SOCK_TIMEOUT, 5000).

-define(RECV_TIMEOUT, 10000).

%% Modbus Socket Options
-define(SOCKOPTS, [
        binary,
        {active, false},
        {packet, 0},
        {reuseaddr, true},
        {nodelay,   true}
]).

%% ============================== exported API =====================================


connect(Host, Port, DeviceName) when  is_list(DeviceName) ->
    connect(Host, Port, list_to_binary(DeviceName));

connect(Host, Port, DeviceName) when  is_binary(DeviceName) ->
    ProcessName = device_to_process(DeviceName),
    gen_server:start_link({local, ProcessName}, ?MODULE, [Host, Port, DeviceName], []).

disconnect(Device) when is_list(Device) ->
    disconnect(list_to_binary(Device));

disconnect(Device) when is_binary(Device) ->
    ProcessName = device_to_process(Device),
    call(ProcessName, stop).

send_request(Device, Req) when is_binary(Device) ->
    gen_server:cast(device_to_process(Device), {send_request, Req, self()});

send_request(Device, Req) when is_atom(Device) ->
    gen_server:cast(device_to_process(Device), {send_request, Req, self()}).


%% ============================== gen_server API =====================================

call(Device, Request) ->
    gen_server:call(Device, Request, ?CALL_TIMEOUT).

init([Host, Port, DeviceName]) ->
    self() ! reconnect,  % DO not call gen_tcp:connect() here, since it may be stuck.
    {ok, #state{socket = undefined, device_name = DeviceName, server_addr = {Host, Port}}}.



handle_call(stop, _From, State=#state{socket = Sock}) ->
	gen_tcp:close(Sock),
	{stop, normal, stopped, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

handle_cast({send_request, Req=#modbus_req{msgid = MsgId}, From}, State)  ->
    case catch request_and_response(Req, State) of
        {'EXIT', Error} ->
            ?LOG(error, "sending Request get error ~p~n", [Error]);
        #modbus_frame{funcode = Funcode, payload = Data} ->
            emq_modbus_control:send_response(From, #modbus_rsp{device_name = State#state.device_name,
                                                                 msgid = MsgId,
                                                                 funcode = Funcode,
                                                                 data = Data});
        {error, Reason} ->
            % If the slave does not receive the query due to a communication error, no response is returned.
            % The master program will eventually process a timeout condition for the query.
            ?LOG(error, "send request get error: ~p~n", [Reason])
    end,
    {noreply, next_id(State)};

handle_cast(Msg, State) ->
    ?LOG(error, "emq_modbus_device, ignore unknow msg=~p~n", [Msg]),
    {noreply, State}.

handle_info(reconnect, State=#state{device_name = DevinceName, server_addr = {Host, Port}}) ->
    case connect_tcp(Host, Port) of
        {ok, Sock} ->
            {noreply, State#state{socket = Sock}};
        {error, _Error} ->
            ?LOG(error, "fail to connect ~p ~p:~p~n", [DevinceName, Host, Port]),
            reconnect_tcp(),
            {noreply, State#state{socket = undefined}}
    end;

handle_info(Info, State) ->
    ?LOG(error, "emq_modbus_device, ignore unknow info=~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%% ============================== internal functions =====================================

connect_tcp(Host, Port) ->
    gen_tcp:connect(Host, Port, ?SOCKOPTS, ?SOCK_TIMEOUT).
    
reconnect_tcp() ->
    erlang:send_after(10, self(), reconnect).


request_and_response(Req, State) ->
    send_and_recv(Req, State).

send_and_recv(_Req, #state{socket = undefined}) ->
    ?LOG(error, "discard request since server has not been connected!~n", []),
    {error, "server has not been connected!"};

send_and_recv(Req, State) ->
    case send(Req, State) of
        ok ->
            case recv(State) of
                {ok, Frame} ->
                    Frame;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "send_and_recv has error, Reason=~p~n", [Reason]),
            {error, Reason}
    end.



send(#modbus_req{funcode = FunCode, data = Data}, #state{tid = Tid, socket = Sock}) ->
    Hdr= #mbap_header{tid = Tid, unit_id = 0},
    Frame = #modbus_frame{header = Hdr, funcode = FunCode, payload = Data},
    ModBusData = serialize(Frame),
    gen_tcp:send(Sock, ModBusData).

serialize(#modbus_frame{header = #mbap_header{tid = Tid, unit_id = Uid},
                          funcode = FunCode, payload = Payload}) ->
    Len = 2 + size(Payload),
    <<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>.
    
recv(State) ->
    case recv(header, State) of
        {ok, Header} ->
            recv(payload, Header, State);
        {error,Error} ->
            {error,Error}
    end.

recv(header, #state{tid = Tid, socket = Sock}) ->
    case gen_tcp:recv(Sock, ?MBAP_LENGTH, ?RECV_TIMEOUT) of
        {ok, <<Tid:16, 0:16, Len:16>>} ->
            {ok, #mbap_header{tid = Tid, length = Len}};
        {ok, Header} ->
            ?LOG(error, "Response cannot match request: request tid=~p, response header =~p", [Tid, Header]),
            {error, badresp};
        {error, Reason} ->
            {error, Reason}
    end.

recv(payload, Header = #mbap_header{length = Len}, #state{socket = Sock}) ->
    case gen_tcp:recv(Sock, Len, ?RECV_TIMEOUT) of
        {ok, <<UnitId:8, FunCodeOrErr:8, Payload/binary>>} ->
            {ok, #modbus_frame{header = Header#mbap_header{unit_id = UnitId},
                               funcode = FunCodeOrErr, payload = Payload}};
        {ok, <<_:8>>}      ->  {error, too_short_modbus_payload};
        {error, Reason}    -> {error, Reason}
    end.

next_id(State = #state{tid = Tid}) when Tid >= 16#EFFF ->
    State#state{tid = 1};

next_id(State = #state{tid = Tid}) ->
    State#state{tid = Tid+1}.

device_to_process(DeviceName) when is_binary(DeviceName) ->
    M = atom_to_binary(?MODULE, utf8),
    ProcessName = <<M/binary, "_", DeviceName/binary>>,
    case catch binary_to_existing_atom(ProcessName, utf8) of
        {'EXIT', _} -> binary_to_atom(ProcessName, utf8);
        Atom -> Atom
    end;

device_to_process(DeviceName) when is_atom(DeviceName) ->
    device_to_process(atom_to_binary(DeviceName, utf8));

device_to_process(DeviceName) when is_list(DeviceName) ->
    device_to_process(list_to_binary(DeviceName)).

