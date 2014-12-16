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
%% command-line entry
main(Args) ->
   {ok, {Opts, Files}} = getopt:parse(opts(), Args),
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
     ,{libdir,    $L, "libdir",  string,           "Library directory"}
     ,{patch,     $p, undefined, undefined,        "Patch module (ephemeral module deployment)"}
     ,{ephemeral, $e, undefined, undefined,        "Ephemeral application deployment"}
     ,{permanent, $d, undefined, undefined,        "Permanent application deployment"}
     ,{system,    $s, undefined, undefined,        "System application deployment"}
     ,{remove,    $r, undefined, undefined,        "Remove deployed application"}
   ].

%%
%% 
init(Opts, Files) ->
   ok = init_libdir(Opts),
   ok = init_net(Opts),
   ok = join_net(Opts),
   ok = lists:foreach(
      fun(File) -> deploy(Opts, File) end,
      Files
   ).

init_libdir(Opts) ->
   case lists:keyfind(libdir, 1, Opts) of
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

deploy(Opts, File) ->
   Path = filename:dirname(File),
   code:add_patha(Path),
   Name = erlang:list_to_atom(
      filename:basename(File, filename:extension(File))
   ),
   ok = maybe_ephemeral(Opts, Name),
   ok = maybe_permanent(Opts, Name),
   ok = maybe_system(Opts, Name),
   ok = maybe_remove(Opts, Name),
   ok = maybe_patch(Opts, Name).
   

maybe_ephemeral(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   case lists:member(ephemeral, Opts) of
      false ->
         ok;
      true  ->
         io:format("==> ephemeral ~s~n", [App]),
         drift:ephemeral(Node, App)
   end.

maybe_permanent(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   case lists:member(permanent, Opts) of
      false ->
         ok;
      true  ->
         io:format("==> permanent ~s~n", [App]),
         drift:permanent(Node, App)
   end.

maybe_system(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   case lists:member(system, Opts) of
      false ->
         ok;
      true  ->
         io:format("==> system ~s~n", [App]),
         drift:permanent(Node, App, [sys])
   end.

maybe_remove(Opts, App) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   case lists:member(remove, Opts) of
      false ->
         ok;
      true  ->
         io:format("==> remove ~s~n", [App]),
         drift:withdraw(Node, App)
   end.

maybe_patch(Opts, Mod) ->
   {_, Node} = lists:keyfind(join, 1, Opts),
   case lists:member(patch, Opts) of
      false ->
         ok;
      true  ->
         io:format("==> patch ~s~n", [Mod]),
         drift:patch(Node, Mod)
   end.
   



