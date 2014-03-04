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

-module(knit_file_win32).


-export([
    rm_rf/1,
    cp_r/2,
    mv/2
]).


-include("knit.hrl").


rm_rf(_) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).


cp_r(_, _) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).


mv(_, _) ->
    ?NOT_SUPPORTED("Windows not currently supported.", []).