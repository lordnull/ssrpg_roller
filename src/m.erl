%% @doc m is for maths!
-module(m).

-include("ssrpg_roller_dice.hrl").

-export([
	success_percent/1,
	success_mean/1, success_percentile/2,
	fail_mean/1, fail_percentile/2,
	threat_mean/1, threat_percentile/2,
	advantage_mean/1, advantage_percentile/2
]).

success_percent(ResList) ->
	Hits = [X || X <- ResList, r:successes(X) > 0],
	length(Hits) / length(ResList) * 100.

success_mean(ResList) ->
	Hits = [X || X <- ResList, r:successes(X) > 0],
	case Hits of
		[] ->
			0;
		_ ->
			Total = lists:foldl(fun(R, A) ->
				A + r:successes(R)
			end, 0, Hits),
			Total / length(Hits)
	end.

success_percentile([], _) ->
	0;

success_percentile(ResList, Percentile) ->
	Success = [r:successes(X) || X <- ResList, r:successes(X) > 0],
	case Success of
		[] ->
			0;
		_ ->
			percentile(Success, Percentile)
	end.

fail_mean(ResList) ->
	Fails = [r:failures(X) || X <- ResList, r:successes(X) < 1],
	case Fails of
		[] ->
			0;
		_ ->
			Total = lists:sum(Fails),
			Total / length(Fails)
	end.

fail_percentile(ResList, Percentile) ->
	Fails = [r:failures(X) || X <- ResList, r:successes(X) < 1],
	case Fails of
		[] ->
			0;
		_ ->
			percentile(Fails, Percentile)
	end.

threat_mean(ResList) ->
	Threats = [r:threats(X) || X <- ResList, r:threats(X) > 0],
	case Threats of
		[] ->
			0;
		_ ->
			Total = lists:sum(Threats),
			Total / length(Threats)
	end.

threat_percentile(ResList, Percentile) ->
	Threats = [r:threats(X) || X <- ResList, r:threats(X) > 0],
	case Threats of
		[] ->
			0;
		_ ->
			percentile(Threats, Percentile)
	end.

advantage_mean(ResList) ->
	Advans = [r:advantages(X) || X <- ResList, r:advantages(X) > 0],
	case Advans of
		[] ->
			0;
		_ ->
			Total = lists:sum(Advans),
			Total / length(Advans)
	end.

advantage_percentile(ResList, Percentile) ->
	Advans = [r:advantages(X) || X <- ResList, r:advantages(X) > 0],
	case Advans of
		[] ->
			0;
		_ ->
			percentile(Advans, Percentile)
	end.

percentile(Numbers, Percentile) ->
	StatDict = lists:foldl(fun(N, Acc) ->
		dict:update_counter(N, 1, Acc)
	end, dict:new(), Numbers),
	Exploded = lists:foldl(fun({N, Times}, Acc) ->
		[N || _ <- lists:seq(1, Times)] ++ Acc
	end, [], dict:to_list(StatDict)),
	Sorted = lists:sort(Exploded),
	KPart = round( (Percentile / 100) * length(Sorted)),
	lists:nth(KPart, Sorted).
