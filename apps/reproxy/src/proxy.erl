%%%-------------------------------------------------------------------
%%% @author alboo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Авг. 2016 14:09
%%%-------------------------------------------------------------------
-module(proxy).
-author("alboo").

-behaviour(gen_server).
-include_lib("reproxy.hrl").

%% API
-export([
  start_link/2,
  get_port/1,
  release/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  client,
  remote,
  port = 0 :: integer(),
  ctrl = 0 :: integer(),
  busy = false :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Client :: gen_tcp:socket(), Port :: integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Client, Port) ->
  gen_server:start_link(?MODULE, [Client, Port], []).


get_port(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, get_port).

release(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, release).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Client, Port]) ->
  ?LOG("Start proxy on port ~p", [Port]),

  create_config(Port),
  run_instance(Port),

  case gen_tcp:connect({127, 0, 0, 1}, Port, ?OPTIONS) of
    {ok, RemoteSocket} ->
      ?DBG("Connection to TOR on port ~p established", [Port]),
      {ok, #state{port = Port, ctrl = Port + 1, client = Client, remote = RemoteSocket}, 0};

    %%communicate(Client, RemoteSocket);
    {error, Error} ->
      ?ERR("Proxy connect error, ~p. 127.0.0.1:~p~n", [Error, Port]),
      gen_tcp:close(Client),
      {stop, server_connect_fail}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(get_port, _From, State) ->
  {reply, State#state.port, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info(timeout, #state{remote = RemoteSocket, client = Client} = State) ->
  ?DBG("Client and server handhake"),
  %% 0x05:version
  case gen_tcp:recv(Client, 0) of
    {ok, Greeting} ->
      ok = gen_tcp:send(RemoteSocket, Greeting),
      {ok, RemoteResponse} = gen_tcp:recv(RemoteSocket, 0),
      gen_tcp:send(Client, RemoteResponse),
      ok = inet:setopts(Client, [{active, true}]),
      ok = inet:setopts(RemoteSocket, [{active, true}]),
      ?DBG("Handshake done. Going to active mode"),
      {noreply, State};

    {error, closed} ->
      {error, client_closed};

    {error, Reason} ->
      {error, Reason}
  end;

handle_info({tcp, Client, Request}, #state{remote = RemoteSocket, client = Client} = State) ->
  case gen_tcp:send(RemoteSocket, Request) of
    ok ->
      {noreply, State};
    {error, _Error} ->
      {stop, _Error, State}
  end;
handle_info({tcp, RemoteSocket, Response}, #state{remote = RemoteSocket, client = Client} = State) ->
  case gen_tcp:send(Client, Response) of
    ok ->
      {noreply, State};
    {error, _Error} ->
      {stop, _Error, State}
  end;
handle_info({tcp_closed, ASocket}, #state{remote = RemoteSocket, client = Client} = State) ->
  case ASocket of
    Client ->
      ?DBG("Client ~p socket closed", [State#state.port]),
      {stop, normal, State};
    RemoteSocket ->
      ?DBG("Proxy ~p socket closed", [State#state.port]),
      {stop, normal, State}
  end;
handle_info({tcp_error, ASocket, Reason}, #state{remote = RemoteSocket, client = Client} = State) ->
  ?DBG("tcp error ~p", [Reason]),
  case ASocket of
    Client ->
      ?WARN("~p client tcp error", [ASocket]),
      {stop, Reason, State};
    RemoteSocket ->
      ?WARN("~p server tcp error", [ASocket]),
      {stop, Reason, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason,  #state{remote = RemoteSocket, client = Client} = State) ->
  gen_tcp:close(RemoteSocket),
  gen_tcp:close(Client),

  (rand:uniform(5) =:= 5) andalso refresh(State#state.ctrl),
  ok;

terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_config(Port) ->
  PortS = integer_to_list(Port),
  CfgDir = config:get('path.data', "/tmp/reproxy") ++ "/" ++ PortS,
  CfgFile = CfgDir ++ "/config",
  case filelib:is_file(CfgFile) of
    true -> ok;
    _ -> create_config_file(Port, CfgDir)
  end,
  ok.


create_config_file(Port, CfgDir) ->
  CfgFile = CfgDir ++ "/config",
  PidFile = CfgDir ++ "/pid",
  DataDir = CfgDir ++ "/data/",

  filelib:ensure_dir(CfgDir),
  filelib:ensure_dir(DataDir),

  PortS = integer_to_list(Port),
  CtrlS = integer_to_list(Port + 1),
  Config = "RunAsDaemon 1\n" ++
           "ControlPort " ++ CtrlS ++ "\n" ++
           "SocksPort " ++ PortS ++ "\n" ++
           "PidFile " ++ PidFile ++ "\n" ++
           "DataDirectory " ++ DataDir ++ "\n" ++
           "HashedControlPassword " ++ config:get('proxy.password_hash', "", string) ++ "\n" ++
           "Log notice file /dev/null\n",
  file:write_file(CfgFile, Config).


run_instance(Port) ->
  PortS = integer_to_list(Port),
  CfgDir = config:get('path.data', "/tmp/reproxy") ++ "/" ++ PortS,
  CfgFile = CfgDir ++ "/config",
  Bin = config:get('proxy.bin'),

  length(os:cmd("ps aux | grep " ++ CfgFile ++ " | grep -v grep")) == 0 andalso
    os:cmd(Bin ++ " -f " ++ CfgFile).


refresh(CtrlPort) ->
  ?DBG("Refresh ~p", [CtrlPort - 1]),
  case gen_tcp:connect({127, 0, 0, 1}, CtrlPort, ?OPTIONS) of
    {ok, RemoteSocket} ->
      Password = list_to_binary(config:get('proxy.password')),
      try
        ok = gen_tcp:send(RemoteSocket, [<<"AUTHENTICATE \"">>, Password, <<"\"\r\n">>]),
        {ok, _} = gen_tcp:recv(RemoteSocket, 0),
        ok = gen_tcp:send(RemoteSocket, <<"signal NEWNYM\r\n">>),
        {ok, _} = gen_tcp:recv(RemoteSocket, 0),
        gen_tcp:close(RemoteSocket)
      catch
          _:_  ->
            ?WARN("Cannot refresh ~p", [CtrlPort - 1])
      end,
      ok;

    {error, Err} ->
      ?WARN("Cannot refresh ~p", [Err]),
      error
  end.

