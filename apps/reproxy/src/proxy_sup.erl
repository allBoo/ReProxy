%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(proxy_sup).
-author("alboo").

-behaviour(supervisor).
-include_lib("reproxy.hrl").

%% API
-export([
  start_link/0,
  start_proxy/2,
  stop_proxy/1,
  stop_all/0,
  list_all/0,
  find_exists/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_proxy(Client, Port) when is_integer(Port) ->
  {ok, Pid} = supervisor:start_child(?SERVER, [Client, Port]),
  Pid.

stop_proxy(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(?SERVER, Pid).

stop_all() ->
  supervisor:terminate_child(reproxy_sup, ?SERVER),
  supervisor:restart_child(reproxy_sup, ?SERVER).


list_all() ->
  [{Pid, proxy:get_port(Pid)} || {_, Pid , _, _} <- supervisor:which_children(?SERVER)].


find_exists() ->
  Processes = string:tokens(os:cmd("ps aux | grep \"tor -f\" | grep -v grep"), "\n"),
  Ports = parse_processes(Processes),
  length(Ports) > 0 andalso ?LOG("Found running proxy processes ~p", [Ports]),
  Ports.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
    {proxy, {proxy, start_link, []}, temporary, 5, worker, [proxy]}
  ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_processes(Processes) -> parse_processes(Processes, []).

parse_processes([], Acc) -> Acc;
parse_processes([Process | Tail], Acc) ->
  T = string:tokens(Process, " "),
  Cfg = string:tokens(lists:nth(length(T), T), "/"),
  Port =
    try
      [list_to_integer(lists:nth(3, Cfg))]
    catch
       _:_ -> []
    end,
  parse_processes(Tail, Acc ++ Port).
