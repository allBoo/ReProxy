%%%-------------------------------------------------------------------
%%% @author alboo
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(env).
-author("alboo").
-include_lib("reproxy.hrl").

%% API
-export([
  init/0,
  get/1,
  get/2,
  set/2,
  app/0,
  app/1
]).


%%--------------------------------------------------------------------
%% @doc
%% Creates env table
%%
%% @end
%%--------------------------------------------------------------------
init() ->
  mnesia:create_table(env, [{disc_copies, [node()]}, {attributes, record_info(fields, env)}]).

%%--------------------------------------------------------------------
%% @doc
%% Get env value
%%
%% @end
%%--------------------------------------------------------------------
get(Key) ->
  case mnesia:transaction(fun() -> mnesia:select(env, [{#env{key = Key, value = '$1'},[], ['$1']}]) end) of
    {atomic, [Value]} -> Value;
    {atomic, []} -> false;
    Any -> Any
  end.

get(Key, Default) ->
  case mnesia:transaction(fun() -> mnesia:select(env, [{#env{key = Key, value = '$1'},[], ['$1']}]) end) of
    {atomic, [Value]} -> Value;
    {atomic, []} -> Default;
    _ -> Default
  end.

%%--------------------------------------------------------------------
%% @doc
%% Set env value
%%
%% @end
%%--------------------------------------------------------------------
set(Key, Value) ->
  mnesia:transaction(fun() -> mnesia:write(#env{key = Key, value = Value}) end).


%%--------------------------------------------------------------------
%% @doc
%% Get application name
%%
%% @end
%%--------------------------------------------------------------------
app() -> app(atom).
app(atom) ->
  {ok, AppName} = application:get_application(),
  AppName;
app(string) ->
  atom_to_list(app(atom)).
