%%
%%   Copyright (c) 2012 - 2013, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%% @todo: patch module
-module(drift).
-include("drift.hrl").

-export([
   ephemeral/2
  ,ephemeral/3
  ,permanent/2
  ,permanent/3
  ,withdraw/2
  ,withdraw/3
  ,deploy/2
  ,patch/2
  ,revert/2
  ,main/1
]).

%%
%% makes ephemeral deployment of application to remote node.
%% The library searches the code path for the application resource 
%% file app.app and load the specification it contains. The application
%% is deployed to VM code memory only. 
%% If application name is omitted then current project is deployed
-spec(ephemeral/2 :: (node(), atom()) -> ok | {error, any()}).
-spec(ephemeral/3 :: (node(), atom(), list()) -> ok | {error, any()}).

ephemeral(Node, App)
 when is_atom(App) ->
   ephemeral(Node, [App], [app]);

ephemeral(Node, App)
 when is_list(App) ->
   ephemeral(Node,  App,  [app]).


ephemeral(Node, [App], [Type]) ->
   %% force deployment of last application
   ephemeral(drift_erl:new(Node, Type, App));

ephemeral(Node, [Head | Tail], [Type]) ->
   %% deploy only missing application
   App = drift_erl:new(Node, Type, Head),
   case drift_erl:exists(App) of
      non_existing ->
         ephemeral(App),
         ephemeral(Node, Tail);
      _ ->
         ephemeral(Node, Tail)
   end;

ephemeral(Node, App, [Type]) ->
   ephemeral(drift_erl:new(Node, Type, App)).

ephemeral(X) ->
   ?DEBUG("==> ephemeral ~s (~s)~n", [drift_erl:name(X), drift_erl:vsn(X)]),
   _ = ephemeral_deploy_appfile(X),
   lists:foreach(
      fun(Mod) -> 
         ephemeral_deploy_module(Mod, X)
      end,
      drift_erl:module(X)
   ).

%%
%% permanently deploys existed application to remote node. The library 
%% searches the code path for the application resource file app.app and
%% load the specification it contains. 
%% If application name is omitted then current project is deployed
-spec(permanent/2 :: (node(), atom()) -> ok | {error, any()}).
-spec(permanent/3 :: (node(), atom(), list()) -> ok | {error, any()}).

permanent(Node, App)
 when is_atom(App) ->
   permanent(Node, [App], [app]);
permanent(Node, App)
 when is_list(App) ->
   permanent(Node,  App,  [app]).

permanent(Node, [App], [Type]) ->
   %% force deployment of last application
   permanent(drift_erl:new(Node, Type, App));

permanent(Node, [Head | Tail], [Type]) ->
   %% deploy only missing application
   App = drift_erl:new(Node, Type, Head),
   case drift_erl:exists(App) of
      non_existing ->
         permanent(App),
         permanent(Node, Tail);
      _ ->
         permanent(Node, Tail)
   end;

permanent(Node, App, [Type]) ->
   permanent(drift_erl:new(Node, Type, App)).

permanent(X) ->
   ?DEBUG("==> permanent ~s (~s)~n", [drift_erl:name(X), drift_erl:vsn(X)]),
   _ = permanent_deploy_appfile(X),
   lists:foreach(
      fun(Mod) -> 
         permanent_deploy_module(Mod, X)
      end,
      drift_erl:module(X)
   ),
   lists:foreach(
      fun(File) ->
         permanent_deploy_private(File, X)
      end,
      drift_erl:private(X)
   ).

%%
%% withdraw deployed application.
-spec(withdraw/2 :: (node(), atom()) -> ok | {error, any()}).
-spec(withdraw/3 :: (node(), atom(), list()) -> ok | {error, any()}).

withdraw(Node, App)
 when is_atom(App) ->
   withdraw(Node, [App], [app]);
withdraw(Node, App)
 when is_list(App) ->
   withdraw(Node,  App,  [app]).

withdraw(Node, [App], [Type]) ->
   withdraw(drift_erl:new(Node, Type, App));

withdraw(Node, [Head | Tail], [Type]) ->
   %% deploy only missing application
   App = drift_erl:new(Node, Type, Head),
   case drift_erl:exists(App) of
      non_existing ->
         withdraw(App),
         withdraw(Node, Tail);
      _ ->
         withdraw(Node, Tail)
   end;

withdraw(Node, App, [Type]) ->
   withdraw(drift_erl:new(Node, Type, App)).


withdraw(X) ->
   ?DEBUG("==> withdraw ~s~n", [drift_erl:name(X)]),
   Node = drift_erl:node(X),
   ok   = lists:foreach(
      fun(Mod) -> 
         ?DEBUG("==> withdraw ~s: module ~s~n", [Node, Mod]),
         _  = rpc:call(Node, code, purge,  [Mod]),
         _  = rpc:call(Node, code, delete, [Mod])
      end,
      drift_erl:module(X)
   ),
   %% attempt to delete application dir
   rpc:call(Node, os, cmd, [
      lists:flatten(io_lib:format("rm -Rf ~s", [drift_erl:path(X)]))
   ]),
   ok.

%%
%% permanently deploy application and its dependencies
-spec(deploy/2 :: (node(), atom()) -> ok | {error, any()}).

deploy(Node, App) ->
   permanent(Node, applib:deps(App)).

%%
%% patch single module
-spec(patch/2 :: (node(), atom()) -> ok | {error, any()}).

patch(Node, Mod)
 when is_atom(Mod) ->
   ?DEBUG("==> patch ~s~n", [Mod]),
   {Mod, Binary, _} = code:get_object_code(Mod),
   _    = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_binary, [Mod, undefined, Binary]),
   ok;

patch(Node, File)
 when is_list(File) ->
   ?DEBUG("==> patch ~s~n", [File]),
   case code:where_is_file(File) of
      non_existing ->
         {error, not_found};

      "/" ++ Suffix = Lpath ->
         {ok, Data} = file:read_file(Lpath),
         Path = filename:join([config(vardir, ?CONFIG_VARDIR), Suffix]),
         ok   = rpc:call(Node, filelib, ensure_dir, [Path]),
         rpc:call(Node, file, write_file, [Path, Data]);

      Lpath        ->
         {ok, Data} = file:read_file(Lpath),
         Path = filename:join([config(vardir, ?CONFIG_VARDIR), Lpath]),
         ok   = rpc:call(Node, filelib, ensure_dir, [Path]),
         rpc:call(Node, file, write_file, [Path, Data])
   end.

%%
%% revert module
-spec(revert/2 :: (node(), atom()) -> ok | {error, any()}).

revert(Node, Mod)
 when is_atom(Mod) ->
   ?DEBUG("==> revert ~s~n", [Mod]),
   rpc:call(Node, code, purge, [Mod]),
   rpc:call(Node, code, delete, [Mod]);

revert(Node, File)
 when is_list(File) ->
   ?DEBUG("==> revert ~s~n", [File]),
   case code:where_is_file(File) of
      non_existing ->
         {error, not_found};

      "/" ++ Suffix = Lpath ->
         Path = filename:join([config(vardir, ?CONFIG_VARDIR), Suffix]),
         rpc:call(Node, file, delete, [Path]);

      Lpath        ->
         Path = filename:join([config(vardir, ?CONFIG_VARDIR), Lpath]),
         rpc:call(Node, file, delete, [Path])
   end.


%%
%% command-line utility
-spec(main/1 :: (list()) -> ok).

main(Args) ->
   drift_tool:main(Args).

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% application config
config(Key, Default) ->
   case application:get_env(drift, Key) of
      undefined -> 
         Default;
      {ok, Val} ->
         Val
   end.

%%
%% ephemeral appfile deployment
ephemeral_deploy_appfile(X) ->
   AppFile = filename:basename(drift_erl:appfile(X)),
   ?DEBUG("==> ephemeral deploy ~s: ~s~n", [drift_erl:name(X), AppFile]),
   case drift_erl:exists(X) of
      non_existing ->
         {ok, Bin} = file:read_file(drift_erl:appfile(X)),
         Node = drift_erl:node(X),
         File = filename:join([drift_erl:path(X), ebin, AppFile]),
         ok   = rpc:call(Node, filelib, ensure_dir, [File]),
         true = rpc:call(Node, code, add_patha, [filename:dirname(File)]),
         ok   = rpc:call(Node, file, write_file, [File, Bin]);
      L ->
         io:format("==> ~p~n", [L]),
         ok
   end.

%%
%% ephemeral appfile deployment
permanent_deploy_appfile(X) ->
   AppFile = filename:basename(drift_erl:appfile(X)),
   ?DEBUG("==> permanent deploy ~s: ~s~n", [drift_erl:name(X), AppFile]),
   {ok, Bin} = file:read_file(drift_erl:appfile(X)),
   Node = drift_erl:node(X),
   File = filename:join([drift_erl:path(X), ebin, AppFile]),
   ok   = rpc:call(Node, filelib, ensure_dir, [File]),
   true = rpc:call(Node, code, add_patha, [filename:dirname(File)]),
   ok   = rpc:call(Node, file, write_file, [File, Bin]).

%%
%% ephemeral module deployment
ephemeral_deploy_module(Mod, X) ->
   ?DEBUG("==> ephemeral deploy ~s: module ~s~n", [drift_erl:name(X), Mod]),
   {Mod, Binary, _} = code:get_object_code(Mod),
   Node = drift_erl:node(X),
   _    = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_binary, [Mod, undefined, Binary]).

   
permanent_deploy_module(Mod, X) ->
   ?DEBUG("==> permanent deploy ~s: module ~s~n", [drift_erl:name(X), Mod]),
   {Mod, Binary, _} = code:get_object_code(Mod),
   Node = drift_erl:node(X),   
   _  = rpc:call(Node, code, ensure_loaded, [Mod]),
   case rpc:call(Node, code, which, [Mod]) of
      % file exists at sandbox, overwrite
      File when is_list(File) ->
         case lists:prefix(drift_erl:path(X), File) of
            true ->
               ok = rpc:call(Node, file, write_file, [File, Binary]),
               _  = rpc:call(Node, code, purge, [Mod]),
               {module, Mod} = rpc:call(Node, code, load_file, [Mod]);
            false ->
               deploy_module(Mod, Binary, X)
         end;
      % file does not exists create new one
      _ ->
         deploy_module(Mod, Binary, X)
   end.

deploy_module(Mod, Binary, X) ->
   File   = filename:join([drift_erl:path(X), ebin, Mod]) ++ code:objfile_extension(),
   % deploy
   Node = drift_erl:node(X),   
   ok = rpc:call(Node, filelib, ensure_dir, [File]),
   ok = rpc:call(Node, file, write_file,    [File, Binary]),
   % load module
   _  = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_file, [Mod]).

%%
%%
permanent_deploy_private(File, X) ->
   ?DEBUG("==> permanent deploy ~s: file ~s", [drift_erl:name(X), File]),
   Suffix = lists:dropwhile(
      fun(A) -> A =/= "priv" end,
      filename:split(File)
   ),
   {ok, Binary} = file:read_file(File),
   Target = filename:join([drift_erl:path(X)] ++ Suffix),

   % deploy module
   Node = drift_erl:node(X),      
   ok   = rpc:call(Node, filelib, ensure_dir, [Target]),
   ok   = rpc:call(Node, file, write_file, [Target, Binary]).

