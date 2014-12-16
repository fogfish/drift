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
  ,deploy/2
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
   try
      ephemeral(drift_erl:new(Node, App))
   catch _:Reason ->
      Reason
   end;

ephemeral(Node, [Tail]) ->
   ephemeral(Node, Tail);
ephemeral(Node, [Head | Tail]) ->
   case drift_erl:exists(drift_erl:new(Node, Head)) of
      %% deploy missed dependency
      non_existing ->
         case ephemeral(Node, Head) of
            ok    ->
               ephemeral(Node, Tail);
            Error ->
               Error
         end;
      %% do not deploy existed dependency   
      _ ->
         ephemeral(Node, Tail)
   end.

ephemeral(Node, App, [Type]) ->
   try
      ephemeral(drift_erl:new(Node, Type, App))
   catch _:Reason ->
      Reason
   end.

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
   try
      permanent(drift_erl:new(Node, App))
   catch _:Reason ->
      Reason
   end;

permanent(Node, [Tail]) ->
   permanent(Node, Tail);
permanent(Node, [Head | Tail]) ->
   case drift_erl:exists(drift_erl:new(Node, Head)) of
      non_existing ->
         case permanent(Node, Head) of
            ok    ->
               permanent(Node, Tail);
            Error ->
               Error
         end;
      _ ->
         permanent(Node, Tail)
   end.

permanent(Node, App, [Type])
 when is_atom(App) ->
   try
      permanent(drift_erl:new(Node, Type, App))
   catch _:Reason ->
      Reason
   end.

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

withdraw(Node, App) ->
   try
      withdraw(drift_erl:new(Node, App))
   catch _:Reason ->
      Reason
   end.

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


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

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
      _ ->
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

