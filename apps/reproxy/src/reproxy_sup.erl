%%%-------------------------------------------------------------------
%% @doc reproxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(reproxy_sup).
-include_lib("reproxy.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{rest_for_one, 5, 10}, [
        ?CHILD(config),
        ?CHILD_SUP(proxy_sup),
        ?CHILD(proxy_pull),
        ?CHILD(server)
    ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
