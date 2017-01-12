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
%%% controller of modbus-tcp gateway..
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emq_modbus_control).

-include("emodbus.hrl").

%% modbus API Exports
-export([start_link/4, stop/0, send_response/2]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        topic_prefix :: binary(),
        qos         :: 0..2,
        retain      :: boolean(),
        tid = 1     :: 1..16#ff
 }).

%% ============================== exported API =====================================

start_link(CompanyName, EdgeName, Qos, Retain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CompanyName, EdgeName, Qos, Retain], []).

stop() ->
    gen_server:stop(?MODULE).

send_response(Control, Response) ->
    gen_server:cast(Control, {send_response, Response, self()}).

%% ============================== gen_server API =====================================

init([CompanyName, EdgeName, Qos, Retain]) ->
    CompanyName1 = list_to_binary(CompanyName),
    EdgeName1 = list_to_binary(EdgeName),
    Topic = <<"/", CompanyName1/binary,  "/modbus_request/", EdgeName1/binary, "/+">>,
    case emq_broker_api:subscribe(Topic) of
        ok ->
            {ok, #state{topic_prefix = <<"/", CompanyName1/binary,  "/modbus_response/", EdgeName1/binary, "/">>,
                         qos = Qos, retain = Retain, tid = 0}};
        {error, Error} ->
            ?LOG(error, "subscribe topic=~p, error=~p~n", [Topic, Error]),
            {stop, Error}
    end.

handle_call(stop, _From, State) ->
    % unsubsribe topics?
	{stop, normal, stopped, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

handle_cast({send_response, Response, _From}, State) ->
    #modbus_rsp{device_name = DeviceName, msgid = MsgId, funcode = Funcode, data = Data} = Response,
    #state{qos = Qos, topic_prefix = TopicPrefix, retain = Retain} = State,
    emq_broker_api:publish(DeviceName, Qos, Retain,
                      <<TopicPrefix/binary, DeviceName/binary>>,
                      <<MsgId:8, Funcode:8, Data/binary>>),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({dispatch, Topic, <<MsgId:8, Code:8, Data/binary>>}, State) ->
    TopicSplited = binary:split(Topic, <<"/">>, [global]),
    DeviceName = lists:last(TopicSplited),
    try
        emq_modbus_device:send_request(DeviceName, #modbus_req{msgid = MsgId, funcode = Code, data = Data})
    catch
        Err -> ?LOG(error, "fail to dispatch message to device=~p, error=~p~n", [DeviceName, Err])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






