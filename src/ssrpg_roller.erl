-module(ssrpg_roller).

-include("ssrpg_roller_dice.hrl").

-define(opt_specs, [
	{skill_pool, $r, "skill", {string, [?ability_die]}, "The skill dice being rolled"},
	{oppossed_pool, $o, "oppossed", {string, [?difficulty_die]}, "The opposing skill dice pool"},
	{soak, $s, "soak", {integer, 0}, "How much soak is applied"},
	{target_hp, $h, "hp", {integer, 12}, "How much hp target has"},
	{weapon_damage, $w, "weapon", {integer, 0}, "How much base damage the weapon does"},
	{iterations, $i, "iterations", {integer, 1000000}, "How many times to do the roll"},
	{ttl, $t, "time-to-live", undefined, "if testing average time to live in rounds, set this"},
	{help, $?, "help", undefined, "Show a usage thing"}
]).

-record(res, {
	success = 0,
	advantage = 0,
	triumph = 0,
	dispair = 0,
	light = 0,
	dark = 0
}).
-record(full_res, {
	success = 0,
	failure = 0,
	advantage = 0,
	threat = 0,
	dispair = 0,
	triumph = 0,
	damage = 0
}).

-export([main/1]).

main(Opts) ->
	random:seed(now()),
	{ok, {OpList, _Tail} = ParsedOpts} = getopt:parse(?opt_specs, Opts),
	case proplists:get_value(help, OpList) of
		undefined ->
			Iters = proplists:get_value(iterations, OpList),
			Skill = proplists:get_value(skill_pool, OpList),
			Difficult = proplists:get_value(oppossed_pool, OpList),
			Soak = proplists:get_value(soak, OpList),
			WeaponDmg = proplists:get_value(weapon_damage, OpList),
			Pool = Skill ++ Difficult,
			FullResList = lists:map(fun(_) ->
				RollRes = roll_pool(Pool),
				Damage = calc_damage(RollRes, WeaponDmg, Soak),
				make_full_res(RollRes, Damage)
			end, lists:seq(1, Iters)),
			HitList = filter_only_hits(FullResList),
			Hits = length(HitList),
			AvgDmg = round( avg_dmg(HitList) * 1000 ) / 1000,
			Percentile80Dmg = percentile_dmg(HitList, 80),
			Percentile50Dmg = percentile_dmg(HitList, 50),
			HitPercent = round( (Hits / length(FullResList) ) * 100000) / 1000,
			ThreatAvg = avg_threat(FullResList),
			AdvanAvg = avg_advantage(FullResList),
			Props = [
				{"Hit %", HitPercent},
				{"Dmg Mean", AvgDmg},
				{"Dmg 50th %", Percentile50Dmg},
				{"Dmg 80th %", Percentile80Dmg},
				{"Threat Mean", ThreatAvg},
				{"Advan Mean", AdvanAvg}
			],
			lists:foreach(fun({K, V}) ->
				io:format("~s: ~p~n", [K, V])
			end, Props);
		_ ->
			getopt:usage(?opt_specs, "ssrpg_roller"),
			io:format("~n"
				"The dice are represented using color~n"
				"    u: boost (blue)~n"
				"    g: ability (green)~n"
				"    y: proficiency (yellow)~n"
				"    b: setback (black)~n"
				"    p: difficulty (purple)~n"
				"    r: challenge (red)~n"
				"~n"
				"So yggprb is 1 profciency, 2 ability, 1 difficulty, 1 challenge, and 1 setback die.~n")
	end.

avg_threat(List) ->
	Ns = lists:map(fun(R) ->
		R#full_res.threat
	end, List),
	avg(Ns).

avg_advantage(List) ->
	Ns = lists:map(fun(R) ->
		R#full_res.advantage
	end, List),
	avg(Ns).

avg([]) ->
	0;
avg(Ns) ->
	Div = length(Ns),
	Sum = lists:sum(Ns),
	round( (Sum / Div) * 1000) / 1000.

avg_dmg(OnlyHits) ->
	Ns = [X || #full_res{damage = X} <- OnlyHits],
	avg(Ns).

filter_only_hits(List) ->
	[R || #full_res{success = X} = R <- List, X > 0].

percentile_dmg(OnlyHits, Percentile) ->
	StatDict = lists:foldl(fun(Res, Acc) ->
		dict:update_counter(Res#full_res.damage, 1, Acc)
	end, dict:new(), OnlyHits),
	Exploded = lists:foldl(fun({Dmg, Times}, Acc) ->
		[Dmg || _ <- lists:seq(1, Times)] ++ Acc
	end, [], dict:to_list(StatDict)),
	Sorted = lists:sort(Exploded),
	KPart = round( (Percentile / 100) * length(Sorted)),
	lists:nth(KPart, Sorted).

print_header(Io) ->
	io:format(Io, "Success, Failure, Advantage, Threat, Triumph, Dispair, Damage~n", []).

print_full_res(Io, FullRes) ->
	io:format(Io, "~p, ~p, ~p, ~p, ~p, ~p, ~p~n", [
		FullRes#full_res.success, FullRes#full_res.failure,
		FullRes#full_res.advantage, FullRes#full_res.threat,
		FullRes#full_res.triumph, FullRes#full_res.dispair,
		FullRes#full_res.damage
	]).

make_full_res(RollRes, Damage) ->
	{Succes, Fail} = if
		RollRes#res.success > 0 ->
			{RollRes#res.success, 0};
		true ->
			{0, abs(RollRes#res.success)}
	end,
	{Advan, Threat} = if
		RollRes#res.advantage > 0 ->
			{RollRes#res.advantage, 0};
		true ->
			{0, abs(RollRes#res.advantage)}
	end,
	#full_res{success = Succes, failure = Fail, advantage = Advan,
		threat = Threat, dispair = RollRes#res.dispair,
		triumph = RollRes#res.triumph, damage = Damage}.

calc_damage(ResRecord, _Weapon, _Soak) when ResRecord#res.success < 1 ->
	0;
calc_damage(ResRecord, Weapon, Soak) ->
	MaybeDmg = ResRecord#res.success + Weapon - Soak,
	if
		MaybeDmg < 0 ->
			0;
		true ->
			MaybeDmg
	end.

roll_pool(DieString) ->
	UncancelledRes = lists:foldl(fun(DieCar, Acc) ->
		Sides = proplists:get_value(DieCar, ?dice_lookup),
		Res = roll_die(Sides),
		lists:append(Acc, Res)
	end, [], DieString),
	make_res_record(UncancelledRes).
	
roll_die(Sides) when is_list(Sides) ->
	Max = length(Sides),
	Nth = random:uniform(Max),
	lists:nth(Nth, Sides).

make_res_record(RawRes) ->
	lists:foldl(fun make_res_record/2, #res{}, RawRes).

make_res_record(?triumph, Rec) ->
	T = Rec#res.triumph,
	Rec#res{triumph = T + 1};
make_res_record(?despair, Rec) ->
	T = Rec#res.dispair,
	Rec#res{dispair = T + 1};
make_res_record(?success, Rec) ->
	N = Rec#res.success,
	Rec#res{success = N + 1};
make_res_record(?failure, Rec) ->
	N = Rec#res.success,
	Rec#res{success = N - 1};
make_res_record(?advantage, Rec) ->
	N = Rec#res.advantage,
	Rec#res{advantage = N + 1};
make_res_record(?threat, Rec) ->
	N = Rec#res.advantage,
	Rec#res{advantage = N - 1};
make_res_record(?light_point, Rec) ->
	N = Rec#res.light,
	Rec#res{light = N + 1};
make_res_record(?dark_point, Rec) ->
	N = Rec#res.dark,
	Rec#res{dark = N + 1}.
	
