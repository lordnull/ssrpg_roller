%% @doc r is short for roll.
-module(r).

-include("ssrpg_roller_dice.hrl").

-export([roll/1, successes/1, failures/1, advantages/1, threats/1,
	triumphs/1, despairs/1, pool/1, from_list/1, from_list/2, to_string/1]).

-record(r, {
	pool = [],
	success = 0,
	fail = 0,
	advantage = 0,
	threat = 0,
	triumph = 0,
	despair = 0
}).

roll(Pool) when is_list(Pool) ->
	R = #r{pool = Pool},
	lists:foldl(fun roll_acc/2, R, Pool).

pool(R) ->
	R#r.pool.

successes(R) ->
	Count = R#r.success - R#r.fail,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

failures(R) ->
	Count = R#r.fail - R#r.success,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

advantages(R) ->
	Count = R#r.advantage - R#r.threat,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

threats(R) ->
	Count = R#r.threat - R#r.advantage,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

triumphs(R) ->
	Count = R#r.triumph - R#r.despair,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

despairs(R) ->
	Count = R#r.despair - R#r.triumph,
	if
		Count < 0 ->
			0;
		true ->
			Count
	end.

from_list(List) ->
	from_list(undefined, List).

from_list(Pool, List) ->
	R = #r{pool = Pool},
	lists:foldl(fun res_acc/2, R, List).

to_string(Rec) ->
	AccessOrder = [despairs, threats, failures, successes, advantages, triumphs],
	lists:foldl(fun(Prop, Acc) ->
		Var = case Prop of
			despairs -> $D;
			threats -> $t;
			failures -> $-;
			successes -> $+;
			advantages -> $a;
			triumphs -> $T
		end,
		Count = ?MODULE:Prop(Rec),
		lists:foldl(fun(_, InnerAcc) ->
			[Var | InnerAcc]
		end, Acc, lists:seq(1, Count))
	end, [], AccessOrder).

% =====
% internal
% =====

roll_acc(Die, Rec) ->
	Res = roll_die(Die),
	lists:foldl(fun res_acc/2, Rec, Res).

roll_die(Die) ->
	Sides = proplists:get_value(Die, ?dice_lookup),
	Nth = random:uniform(length(Sides)),
	lists:nth(Nth, Sides).

res_acc(L, Rec) when is_list(L) ->
	lists:foldl(fun res_acc/2, Rec, L);

res_acc(?triumph, Rec) ->
	T = Rec#r.triumph,
	Rec#r{triumph = T + 1};
res_acc(?despair, Rec) ->
	T = Rec#r.despair,
	Rec#r{despair = T + 1};
res_acc(?success, Rec) ->
	N = Rec#r.success,
	Rec#r{success = N + 1};
res_acc(?failure, Rec) ->
	N = Rec#r.success,
	Rec#r{success = N - 1};
res_acc(?advantage, Rec) ->
	N = Rec#r.advantage,
	Rec#r{advantage = N + 1};
res_acc(?threat, Rec) ->
	N = Rec#r.advantage,
	Rec#r{advantage = N - 1}.
	
