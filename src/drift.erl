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
%%  @description
%%
-module(drift).
-include("drift.hrl").

-export([
   ephemeral/1
  ,ephemeral/2
  ,permanent/1
  ,permanent/2
  ,withdraw/1
  ,withdraw/2
]).

%%
%% makes ephemeral deployment of application to remote node.
%% The library searches the code path for the application resource 
%% file app.app and load the specification it contains. The application
%% is deployed to VM code memory only. 
%% If application name is omitted then current project is deployed
-spec(ephemeral/1 :: (node()) -> ok | {error, any()}).
-spec(ephemeral/2 :: (node(), atom()) -> ok | {error, any()}).

ephemeral(Node) ->
   {ok, Path} = file:get_cwd(),
   App = list_to_atom(filename:basename(Path)),
   ephemeral(Node, App, Path).

ephemeral(Node, App)
 when is_atom(App) ->
   ephemeral(Node, App, code:lib_dir(App));
ephemeral(Node, [Head|Tail]) ->
   ephemeral(Node, Head),
   ephemeral(Node, Tail);
ephemeral(_Node, []) ->
   ok.

ephemeral(Node, App, Path) ->
   ?DEBUG("==> ephemeral ~s ~s", [Node, Path]),
   _ = ephemeral_deploy_appfile(Node, App, Path),
   lists:foreach(
      fun(Mod) -> 
         ephemeral_deploy_module(Node, Mod, Path)
      end,
      modules(Path)
   ).

%%
%% permanently deploys existed application to remote node. The library 
%% searches the code path for the application resource file app.app and
%% load the specification it contains. 
%% If application name is omitted then current project is deployed
-spec(permanent/1 :: (node()) -> ok | {error, any()}).
-spec(permanent/2 :: (node(), atom()) -> ok | {error, any()}).

permanent(Node) ->
   {ok, Path} = file:get_cwd(),
   App = list_to_atom(filename:basename(Path)),
   permanent(Node, App, Path).

permanent(Node, App)
 when is_atom(App) ->
   permanent(Node, App, code:lib_dir(App));
permanent(Node, [Head|Tail]) ->
   permanent(Node, Head),
   permanent(Node, Tail);
permanent(_Node, []) ->
   ok.


permanent(Node, App, Path) ->
   ?DEBUG("==> permanent ~s ~s~n", [Node, Path]),
   Root = permanent_deploy_appfile(Node, App, Path),
   lists:foreach(
      fun(Mod) -> 
         permanent_deploy_module(Node, Mod, Root)
      end,
      modules(Path)
   ),
   lists:foreach(
      fun(File) ->
         permanent_deploy_private(Node, File, Root)
      end,
      privates(Path)
   ).

%%
%% withdraw deployed application.
-spec(withdraw/2 :: (node(), atom()) -> ok | {error, any()}).

withdraw(Node) ->
   {ok, Path} = file:get_cwd(),
   App = list_to_atom(filename:basename(Path)),
   withdraw(Node, App, Path).

withdraw(Node, App) ->
   withdraw(Node, App, code:lib_dir(App)).

withdraw(Node, App, Path) ->
   ?DEBUG("==> withdraw ~s ~s~n", [Node, Path]),
   ok   = lists:foreach(
      fun(Mod) -> 
         ?DEBUG("==> withdraw ~s: module ~s~n", [Node, Mod]),
         _  = rpc:call(Node, code, purge,  [Mod]),
         _  = rpc:call(Node, code, delete, [Mod])
      end,
      modules(Path)
   ),
   %% attempt to delete application dir
   Root = path(Node, App),
   rpc:call(Node, os, cmd, [lists:flatten(io_lib:format("rm -Rf ~s", [Root]))]),
   ok.



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
%% list of binary files
files(Path) ->
   filelib:wildcard(
      filename:join([Path, ebin, "*" ++ code:objfile_extension()])
   ).

%%
%% list of compiled modules
modules(Path) ->
   [list_to_atom(filename:basename(X, code:objfile_extension())) || X <- files(Path)].

%%
%% list of private files
%% @todo: recursive
privates(Path) ->
   fs:fold(
      fun(X, Acc) -> [X|Acc] end,
      [],
      filename:join([Path, priv])
   ).

%%
%% application node path
path(Node, App) ->
   case rpc:call(Node, code, lib_dir, [App]) of
      %% application is not known by node
      {error, _} ->
         filename:join([config(libdir, ?CONFIG_LIBDIR), Node, "lib", App]);
      Result ->
         Result
   end.

%%
%% check if application is loaded or running on remote node
is_application(Node, App) ->
   Running = rpc:call(Node, application, which_applications, []),
   case lists:keyfind(App, 1, Running) of
      false ->
         Loaded = rpc:call(Node, application, loaded_applications, []),
         lists:keyfind(App, 1, Loaded);
      Result->
         Result
   end.


%%
%% ephemeral appfile deployment
ephemeral_deploy_appfile(Node, App, Path) ->
   ?DEBUG("==> ephemeral deploy ~s: ~s~n", [Node, App]),
   File  = atom_to_list(App) ++ ".app",
   Local = filename:join([Path, ebin, File]),
   {ok, Binary} = file:read_file(Local),
   case is_application(Node, App) of
      %% application is not known (appfile shall be uploaded)
      false ->
         Remote = filename:join([path(Node, App), ebin, File]),
         ok   = rpc:call(Node, filelib, ensure_dir, [Remote]),
         true = rpc:call(Node, code, add_patha, [filename:dirname(Remote)]),
         ok   = rpc:call(Node, file, write_file, [Remote, Binary]);
      %% application is know hotswap modules   
      _ ->
         ok
   end.

%%
%% ephemeral appfile deployment
permanent_deploy_appfile(Node, App, Path) ->
   ?DEBUG("==> permanent deploy ~s: ~s~n", [Node, App]),
   File  = atom_to_list(App) ++ ".app",
   Local = filename:join([Path, ebin, File]),
   {ok, Binary} = file:read_file(Local),
   Root   = path(Node, App),
   Remote = filename:join([Root, ebin, File]),
   ok   = rpc:call(Node, filelib, ensure_dir, [Remote]),
   true = rpc:call(Node, code, add_patha, [filename:dirname(Remote)]),
   ok   = rpc:call(Node, file, write_file, [Remote, Binary]),
   Root.

%%
%% ephemeral module deployment
ephemeral_deploy_module(Node, Mod, _Path) ->
   ?DEBUG("==> ephemeral deploy ~s: module ~s~n", [Node, Mod]),
   {Mod, Binary, _} = code:get_object_code(Mod),
   _  = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_binary, [Mod, undefined, Binary]).

   
permanent_deploy_module(Node, Mod, Root) ->
   ?DEBUG("==> permanent deploy ~s: module ~s~n", [Node, Mod]),
   {Mod, Binary, _} = code:get_object_code(Mod),
   _  = rpc:call(Node, code, ensure_loaded, [Mod]),
   case rpc:call(Node, code, which, [Mod]) of
      % file exists (overwrite it)
      File when is_binary(File) orelse is_list(File) ->
         % load module
         ok = rpc:call(Node, file, write_file, [File, Binary]),
         _  = rpc:call(Node, code, purge, [Mod]),
         {module, Mod} = rpc:call(Node, code, load_file, [Mod]);
      % file does not exists create new one
      _ ->
         File   = filename:join([Root, ebin, Mod]) ++ code:objfile_extension(),
         % deploy module
         ok = rpc:call(Node, filelib, ensure_dir, [File]),
         ok = rpc:call(Node, file, write_file, [File, Binary]),

         % load module
         _  = rpc:call(Node, code, purge, [Mod]),
         {module, Mod} = rpc:call(Node, code, load_file, [Mod])
   end.

permanent_deploy_private(Node, File, Root) ->
   ?DEBUG("==> permanent deploy ~s: file ~s", [Node, File]),

   {ok, Binary} = file:read_file(File),
   Target = filename:join([Root, priv, filename:basename(File)]),

   % deploy module
   ok   = rpc:call(Node, filelib, ensure_dir, [Target]),
   ok   = rpc:call(Node, file, write_file, [Target, Binary]).

