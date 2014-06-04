%% @doc g is for generate.
-module(g).

-include("ssrpg_roller_dice.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([all_possible/1, generate/1]).

all_possible(Dice) ->
	Lookup = dict:from_list(?dice_lookup),
	Sides = lists:map(fun(D) ->
		dict:fetch(D, Lookup)
	end, Dice),
	Generated = generate(Sides),
	simplify(Generated).

simplify(SidesUp) ->
	lists:map(fun(NotFlat) ->
		lists:flatten(NotFlat)
	end, SidesUp).

generate([Sides | Tail]) ->
	ProperAcc = lists:map(fun(S) ->
		[S]
	end, Sides),
	generate(Tail, ProperAcc).

generate([], Acc) ->
	Acc;

generate([Sides | Tail], Acc) ->
	Accs = lists:map(fun(Side) ->
		lists:map(fun(Res) ->
			[Side | Res]
		end, Acc)
	end, Sides),
	NewAcc = lists:concat(Accs),
	generate(Tail, NewAcc).

-ifdef(TEST).

generate_test() ->
	Sides = [[1,2], [1,2], [1,2]],
	Got = generate(Sides),
	?debugFmt("Der gots: ~p", [Got]),
	Expected = [[1,1, 1], [1, 1, 2], [1, 2,1], [1, 2,2], [2, 1, 1],
		[2, 1, 2], [2, 2, 1], [2, 2, 2]],
	?assertEqual(length(Expected), length(Got)),
	lists:foreach(fun(E) ->
		?assert(lists:member(E, Got))
	end, Expected).

-endif.
