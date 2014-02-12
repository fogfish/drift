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

%% enable debug output
-define(CONFIG_DEBUG, true).

%%
%% default libdir
-define(CONFIG_LIBDIR, "/tmp/drift").

%%
%% logger macro
-ifndef(ERROR).
-define(ERROR(Fmt, Args), error_logger:error_msg(Fmt, Args)).
-endif.

-ifndef(DEBUG).
   -ifdef(CONFIG_DEBUG).
      -define(DEBUG(Fmt, Args), error_logger:info_msg(Fmt, Args)).
   -else.
      -define(DEBUG(Fmt, Args), ok).
   -endif.
-endif.



