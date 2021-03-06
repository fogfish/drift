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
%%   erlang application 
-module(drift_erl).
-include("drift.hrl").

-export([
   new/2
  ,new/3
  ,node/1
  ,name/1
  ,vsn/1
  ,appfile/1
  ,binary/1
  ,module/1
  ,private/1
  ,exists/1
  ,path/1
]).

%% internal state
-record(app, {
   node    = undefined :: node()       %% node name where to deploy application
  ,type    = undefined :: sys | app    %% type of application sys - overwrites (sys library)
  ,name    = undefined :: atom()       %% application name
  ,appfile = undefined :: list()
  ,version = undefined :: list()
}).

%%
%%
new(Node, Name) ->
   new(Node, app, Name).

new(Node, Type, Name) ->
   File = lookup(Name),
   #app{
      node    = Node
     ,type    = Type
     ,name    = Name
     ,appfile = File
     ,version = version(File)
   }.

%%
%%
node(#app{node=X}) -> 
   X.

%%
%%
name(#app{name=X}) -> 
   X.

%%
%%
vsn(#app{version=X}) -> 
   X.

%%
%%
appfile(#app{appfile=X}) -> 
   X.

%%
%% list of binary files
binary(#app{appfile=X}) ->
   filelib:wildcard(
      filename:join([filename:dirname(X), "*" ++ code:objfile_extension()])
   ).

%%
%% list of compiled modules
module(#app{}=X) ->
   [list_to_atom(filename:basename(File, code:objfile_extension())) || File <- binary(X)].

%%
%% list of private files
private(#app{appfile=X}) ->
   fs:fold(
      fun(File, Acc) -> [File|Acc] end,
      [],
      filename:join([filename:dirname(filename:dirname(X)), priv])
   ).


%%
%% check if application exists on remote node
exists(#app{node=Node, name=Name}) ->
   lookup(Node, Name).

%%
%% remote path
path(#app{type=app, name=Name, version=Vsn}) ->
   filename:join([config(libdir, ?CONFIG_LIBDIR), scalar:c(Name) ++ "-" ++ Vsn]);

path(#app{node=Node, type=sys, name=Name, version=Vsn}) ->
   Root = rpc:call(Node, code, lib_dir, []),
   filename:join([Root, scalar:c(Name) ++ "-" ++ Vsn]).


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
%% lookup application
lookup(Name) ->
   File = scalar:c(Name) ++ ".app",
   code:where_is_file(File).

lookup(Node, Name) ->
   File = scalar:c(Name) ++ ".app",
   rpc:call(Node, code, where_is_file, [File]).

%%
%% read version
version(non_existing) ->
   undefined;
version(File) ->
   {ok, [{application, _, Opts}]} = file:consult(File),
   {vsn, Vsn} = lists:keyfind(vsn, 1, Opts),
   Vsn.

