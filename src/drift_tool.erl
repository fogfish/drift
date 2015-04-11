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
-define(verbose(Msg, Args, Opts),  lists:member(verbose, Opts) =/= undefined andalso io:format(Msg, Args)).

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
     ,{libdir,    $L, "libdir",  string,           "Root path to lookup libraries (expanded to path/*/ebin)"}
     ,{prefix,    $P, "prefix",  string,           "Prefix to install libraries"}
     ,{deps,      $d, undefined, undefined,        "Install all application dependencies"}
     ,{system,    $s, undefined, undefined,        "System flag, installs library to code:lib_dir()"}
     ,{permanent, $p, undefined, undefined,        "Permanent flag, application code is written to disk"}
     ,{remove,    $r, undefined, undefined,        "Remove flag, application code is withdrawn from node"}
     ,{patch,     $m, undefined, undefined,        "Patch module (ephemeral module deployment)"}
     ,{verbose,   $v, undefined, undefined,        "Verbose output"}
   ].

%%
%% 
init(Opts, Files) ->
   ok = init_prefix(Opts),
   ok = init_net(Opts),
   ok = join_net(Opts),
   ok = lists:foreach(
      fun(File) -> exec(Opts, File) end,
      Files
   ).

init_prefix(Opts) ->
   case lists:keyfind(prefix, 1, Opts) of
      false ->
         ok;
      {_, LibDir} ->
         application:set_env(drift, libdir, LibDir)
   end.

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

exec(Opts, File) ->
   add_code_path(Opts, File),
   Name = erlang:list_to_atom(
      filename:basename(File, filename:extension(File))
   ),
   case lists:member(patch, Opts) of
      false ->
         case lists:member(remove, Opts) of
            false ->
               deploy(Opts, Name);
            true  ->
               remove(Opts, Name)
         end;
      true  ->
         patch(Opts, Name)
   end.

%%
%% patch single module only
patch(Opts, Mod) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   io:format("==> patch ~s~n", [Mod]),
   drift:patch(Node, Mod).

%%
%% remove application
remove(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   io:format("==> remove ~s~n", [App]),
   drift:withdraw(Node, App).

%%
%% deploy application
deploy(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   List = case lists:member(deps, Opts) of
      false ->
         [App];
      true  ->
         applib:deps(App)
   end,
   case lists:member(system, Opts) of
      false ->
         case lists:member(permanent, Opts) of
            false ->
               io:format("==> ephemeral ~s~n", [App]),
               drift:ephemeral(Node, List);
            true  ->
               io:format("==> permanent ~s~n", [App]),
               drift:permanent(Node, List)
         end;
      true  ->
         io:format("==> system ~s~n", [App]),
         drift:permanent(Node, List, [sys])
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
      [X || {libdir, X} <- Opts]
   ).







   



