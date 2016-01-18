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
%%   drift command line utility
-module(drift_tool).

-export([main/1]).

%%
%%
-define(verbose(Msg, Args, Opts),  lists:member(verbose, Opts) =/= false andalso io:format(Msg, Args)).

%%
%% command-line entry
main(Args) ->
   {ok, {Opts, Files}} = getopt:parse(opts(), Args),
   lists:foreach(
      fun(X) -> ?verbose("::: option ~p~n", [X], Opts) end, 
      Opts
   ),
   lists:foreach(
      fun(X) -> ?verbose("::: file ~p~n", [X], Opts) end, 
      Files
   ),
   case lists:member(help, Opts) of
      true ->
         getopt:usage(opts(), escript:script_name(), "FILE"),
         halt(0);
      _    ->
         init(Opts, Files),
         halt(0)
   end.

%%
%% command line options
opts() ->
   [
      {help,      $h, "help",    undefined,        "Print usage"}
     ,{join,      $J, "join",    atom,             "Erlang long node name of remote node"}
     ,{cookie,    $C, "cookie",  {atom, nocookie}, "Erlang network distribution magic cookie"}

     ,{libdir,    $L, "libdir",  string,           "Root path to install libraries on remote node"}
     ,{vardir,    $V, "vardir",  string,           "Root path to install data on remote node"}
     ,{path,      $P, "path",    string,           "Root path to lookup libraries (expanded to path/*/ebin)"}

     ,{app,       $a, undefined, undefined,        "Application deployment flag"}
     ,{file,      $f, undefined, undefined,        "Files deployment flag"}

     ,{permanent, $p, undefined, undefined,        "Permanently deployment flag"}
     ,{delete,    $d, undefined, undefined,        "Remove files flag"}

     ,{system,    $s, undefined, undefined,        "System code deployment flag using code:lib_dir()"}
     ,{recursive, $r, undefined, undefined,        "Recursive deployment (application deps)"}
     ,{verbose,   $v, undefined, undefined,        "Verbose output"}
   ].

%%
%% 
init(Opts, Files) ->
   ok = init_env(Opts),
   ok = init_net(Opts),
   ok = join_net(Opts),
   ok = lists:foreach(fun(File) -> exec(Opts, File) end, Files).

%%
%%
init_env(Opts) ->
   init_env_libdir(Opts),
   init_env_vardir(Opts).

init_env_libdir(Opts) ->
   case lists:keyfind(libdir, 1, Opts) of
      false ->
         ok;
      {_, LibDir} ->
         application:set_env(drift, libdir, LibDir)
   end.

init_env_vardir(Opts) ->
   case lists:keyfind(vardir, 1, Opts) of
      false ->
         ok;
      {_, LibDir} ->
         application:set_env(drift, vardir, LibDir)
   end.

%%
%%
init_net(Opts) ->
   Node = erlang:list_to_atom(filename:basename(escript:script_name()) ++ "@127.0.0.1"),
   {_, Cookie} = lists:keyfind(cookie, 1, Opts),
   net_kernel:start([Node, longnames]),
   erlang:set_cookie(Node, Cookie),
   io:format("==> init ~s~n", [Node]).

join_net(Opts) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   pong = net_adm:ping(Node),
   io:format("==> join ~s~n", [Node]).

%%
%%
exec(Opts, File) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   add_code_path(Opts, File),
   case lists:member(permanent, Opts) of
      false ->
         case lists:member(delete, Opts) of
            false ->
               exec(ephemeral, type(Opts), Node, Opts, File);
            true  ->
               exec(delete, type(Opts), Node, Opts, File)
         end;      
      true  ->
         exec(permanent, type(Opts), Node, Opts, File)
   end.

exec(permanent, app,  Node, Opts, File) ->
   io:format("==> permanent ~s~n", [File]),
   drift:permanent(Node, apps(Opts, File), flags(Opts));

exec(permanent, file, Node, _Opts, File) ->
   io:format("==> patch ~s~n", [File]),
   case filename:extension(File) of
      ".beam" ->
         drift:patch(Node, mod(File));
      []      ->
         drift:patch(Node, mod(File));
      _       ->
         drift:patch(Node, filename:basename(File))
   end;

exec(ephemeral, app,  Node, Opts, File) ->
   io:format("==> ephemeral ~s~n", [File]),
   drift:ephemeral(Node, apps(Opts, File));

exec(ephemeral, file, Node, _Opts, File) ->
   io:format("==> patch ~s~n", [File]),
   case filename:extension(File) of
      ".beam" ->
         drift:patch(Node, mod(File));
      []      ->
         drift:patch(Node, mod(File));
      _       ->
         drift:patch(Node, filename:basename(File))
   end;

exec(delete, app,  Node, Opts, File) ->
   io:format("==> remove ~s~n", [File]),
   drift:withdraw(Node, apps(Opts, File));

exec(delete, file, Node, _Opts, File) ->
   io:format("==> remove ~s~n", [File]),
   case filename:extension(File) of
      ".beam" ->
         drift:revert(Node, mod(File));
      []      ->
         drift:revert(Node, mod(File));
      _       ->
         drift:revert(Node, filename:basename(File))
   end.

%%
%%
type(Opts) ->
   case lists:member(app, Opts) of
      true  ->
         app;
      false ->
         case lists:member(file, Opts) of
            true  ->
               file;
            false ->
               undefined
         end
   end.

%%
%% list of apps to install
apps(Opts, File) ->
   App = erlang:list_to_atom(
      filename:basename(File, filename:extension(File))
   ),
   case lists:member(recursive, Opts) of
      false ->
         [App];
      true  ->
         applib:deps(App)
   end.

mod(File) ->
   erlang:list_to_atom(
      filename:basename(File, filename:extension(File))
   ).

%%
%%
flags(Opts) ->
   case lists:member(system, Opts) of
      false ->
         [app];
      true  ->
         [sys]
   end.

%%
%%
add_code_path(Opts, File) ->
   Path = filename:dirname(File),
   code:add_patha(Path),
   ?verbose("::: add path ~s~n", [Path], Opts),
   lists:foreach(
      fun(LibDir) ->
         lists:foreach(
            fun(X) ->
               code:add_patha(X),
               ?verbose("::: add path ~s~n", [X], Opts)
            end,
            filelib:wildcard(filename:join([LibDir, "*", "ebin"]))
         )
      end,
      [X || {path, X} <- Opts]
   ).







   



