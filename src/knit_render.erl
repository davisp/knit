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

-module(knit_render).

-export([
    resolve/1,
    run/2
]).


resolve(Ctx) ->
    resolve(dict:to_list(Ctx), Ctx).


resolve([], Ctx) ->
    Ctx;
resolve([{Key, Value0} | Rest], Ctx) when is_list(Value0) ->
    Value = run(list_to_binary(Value0), Ctx),
    resolve(Rest, dict:store(Key, Value, Ctx));
resolve([_Pair | Rest], Ctx) ->
    resolve(Rest, Ctx).


run(Tmpl0, Ctx) ->
    %% Be sure to escape any double-quotes before rendering...
    Tmpl1 = re:replace(Tmpl0, "\\\\", "\\\\\\", [global, {return, list}]),
    Tmpl2 = re:replace(Tmpl1, "\"", "\\\\\"", [global, {return, list}]),
    mustache:render(Tmpl2, Ctx).

