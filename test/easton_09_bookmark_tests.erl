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

-module(easton_09_bookmark_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/09".


open_idx() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    {ok, Idx} = easton_index:open(idx_dir()),
    lists:foreach(fun({Id, Shape}) ->
        ok = easton_index:update(Idx, Id, Shape)
    end, idx_objs()),
    Idx.


close_idx(Idx) ->
    easton_index:close(Idx).


bookmark_test_() ->
    {"Bookmark Tests",
        {timeout, 10, [
            {setup,
                fun open_idx/0,
                fun close_idx/1,
                fun(Idx) -> {with, Idx, [
                    fun get_bookmark_test/1,
                    fun page_bookmark_test/1
                ]} end
            }
        ]}
    }.


get_bookmark_test(Idx) ->
    {ok, [Result1]} = easton_index:search(Idx, qobj(), [
            {limit, 1}
        ]),
    {ok, [Result2]} = easton_index:search(Idx, qobj(), [
            {bookmark, Result1},
            {limit, 1}
        ]),
    ?assertNotEqual(Result1, Result2),
    ResultIds = [Id || {Id, _} <- [Result1, Result2]],
    ?assertEqual(ResultIds, lists:sublist(idx_obj_ids(), 2)).


page_bookmark_test(Idx) ->
    Limits = lists:seq(1, length(idx_objs()) - 1),
    lists:foreach(fun(Limit) ->
        run_page_test(Idx, Limit)
    end, Limits).


run_page_test(Idx, Limit) ->
    TotalPages0 = length(idx_objs()) div Limit,
    TotalPages1 = case length(idx_objs()) rem Limit of
        0 -> 0;
        _ -> 1
    end + TotalPages0,
    PageNums = lists:seq(1, TotalPages1),

    {_, Results} = lists:foldl(fun(_, {BM, Acc}) ->
        Opts = if BM == undefined -> []; true ->
            [{bookmark, BM}]
        end ++ [{limit, Limit}],
        {ok, Page} = easton_index:search(Idx, qobj(), Opts),
        NextBM = lists:last(Page),
        {NextBM, Acc ++ Page}
    end, {undefined, []}, PageNums),

    Ids = [Id || {Id, _} <- lists:flatten(Results)],
    ?assertEqual(Ids, idx_obj_ids()).


qobj() ->
    easton_shapes:rectangle(-180, -90, 180, 90).


idx_obj_ids() ->
    [Id || {Id, _} <- idx_objs()].


idx_objs() ->
    [
        {<<"a">>, easton_shapes:point(0.0, 0.0)},
        {<<"b">>, easton_shapes:point(0.0, 0.0)},
        {<<"c asfaf">>, easton_shapes:point(0.0, 0.0)},
        {<<"d">>, easton_shapes:point(0.0, 0.0)},

        {<<"e">>, easton_shapes:point(1.0, 0.0)},
        {<<"f stuff">>, easton_shapes:point(1.0, 0.0)},
        {<<"g">>, easton_shapes:point(1.0, 0.0)},
        {<<"h boop">>, easton_shapes:point(1.0, 0.0)},
        {<<"i deboop">>, easton_shapes:point(1.0, 0.0)},

        {<<"j">>, easton_shapes:point(2.0, 0.0)},
        {<<"k">>, easton_shapes:point(2.0, 0.0)},

        {<<"l makdf">>, easton_shapes:point(3.0, 0.0)},
        {<<"m asdf">>, easton_shapes:point(3.0, 0.0)},
        {<<"n wrwers">>, easton_shapes:point(3.0, 0.0)},

        {<<"o dfewr">>, easton_shapes:point(3.0, 0.0)},

        {<<"p">>, easton_shapes:point(4.0, 0.0)},
        {<<"q">>, easton_shapes:point(4.0, 0.0)},
        {<<"r">>, easton_shapes:point(4.0, 0.0)},
        {<<"s">>, easton_shapes:point(4.0, 0.0)},

        {<<"t">>, easton_shapes:point(5.0, 0.0)},

        {<<"world">>, easton_shapes:point(6.0, 0.0)},
        {<<"world hello">>, easton_shapes:point(6.0, 0.0)},
        {<<"world hello prefix yay">>, easton_shapes:point(6.0, 0.0)}
    ].
