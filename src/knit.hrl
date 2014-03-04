% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(BAD_CLI(Msg, Args), knit_util:abort(1, Msg, Args)).
-define(BAD_CONFIG(Msg, Args), knit_util:abort(2, Msg, Args)).
-define(IO_ERROR(Msg, Args), knit_util:abort(3, Msg, Args)).
-define(NOT_SUPPORTED(Msg, Args), knit_util:abort(126, Msg, Args)).
-define(ABORT(Msg, Args), knit_util:abort(127, Msg, Args)).

-define(TRACE(Msg), knit_log:trace(Msg, ?FILE, ?LINE)).
-define(TRACE(Msg, Args), knit_log:trace(Msg, Args, ?FILE, ?LINE)).
