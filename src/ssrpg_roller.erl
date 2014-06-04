-module(ssrpg_roller).

-include("ssrpg_roller_dice.hrl").

-define(opt_specs, [
	{skill_pool, $r, "skill", {string, [?ability_die]}, "The skill dice being rolled"},
	{monty, $m, "monty", undefined, "Use monty carlo rather than generated roll chances"},
	{iterations, $i, "iterations", {integer, 1000000}, "How many times to do the roll when using monty"},
	{help, $?, "help", undefined, "Show a usage thing"}
]).

-export([main/1]).

main(Opts) ->
	random:seed(now()),
	{ok, {OpList, _Tail} = ParsedOpts} = getopt:parse(?opt_specs, Opts),
	case proplists:get_value(help, OpList) of
		undefined ->
			Iters = proplists:get_value(iterations, OpList),
			Pool = fix_pool(proplists:get_value(skill_pool, OpList)),
			%Soak = proplists:get_value(soak, OpList),
			%WeaponDmg = proplists:get_value(weapon_damage, OpList),
			FullResList = case proplists:get_value(monty, OpList) of
				undefined ->
					lists:map(fun(R) ->
						r:from_list(Pool, R)
					end, g:all_possible(Pool));
				_ ->
					lists:map(fun(_) ->
						RollRes = r:roll(Pool),
						%Damage = calc_damage(RollRes, WeaponDmg, Soak),
						RollRes
					end, lists:seq(1, Iters))
			end,
			Props = [
				{"Success %", m:success_percent(FullResList)},
				{"Success Mean", m:success_mean(FullResList)},
				{"Success 50%", m:success_percentile(FullResList, 50)},
				{"Success 80%", m:success_percentile(FullResList, 80)},
				{"Success max", m:success_percentile(FullResList, 100)},
				{"Fail Mean", m:fail_mean(FullResList)},
				{"Fail 50%", m:fail_percentile(FullResList, 50)},
				{"Fail 80%", m:fail_percentile(FullResList, 80)},
				{"Fail max", m:fail_percentile(FullResList, 100)},
				{"Threat %", m:threat_percent(FullResList)},
				{"Threat Mean", m:threat_mean(FullResList)},
				{"Threat 50%", m:threat_percentile(FullResList, 50)},
				{"Threat 80%", m:threat_percentile(FullResList, 80)},
				{"Threat max", m:threat_percentile(FullResList, 100)},
				{"Advantage %", m:advantage_percent(FullResList)},
				{"Advantage Mean", m:advantage_mean(FullResList)},
				{"Advantage 50%", m:advantage_percentile(FullResList, 50)},
				{"Advantage 80%", m:advantage_percentile(FullResList, 80)},
				{"Advantage max", m:advantage_percentile(FullResList, 100)},
				{"Triumph %", m:triumph_percent(FullResList)},
				{"Despair %", m:despair_percent(FullResList)}
			],
			lists:foreach(fun({K, V}) ->
				io:format("~s: ~p~n", [K, V])
			end, Props);
		_ ->
			getopt:usage(?opt_specs, "ssrpg_roller"),
			io:format(""
"Dice are represented using the first letter of the name of the die. You~n"
"can also use the color of the die if you choose to. The colors are~n"
"represented by a capital letter:~n"
				"    U: boost (blue)~n"
				"    G: ability (green)~n"
				"    Y: proficiency (yellow)~n"
				"    B: setback (black)~n"
				"    P: difficulty (purple)~n"
				"    R: challenge (red)~n"
				"~n"
"So YGGPRB and paadcs is 1 profciency, 2 ability, 1 difficulty, 1 challenge,"
"and 1 setback die.~n")
	end.

fix_pool(In) ->
	lists:map(fun
		($U) -> ?boost_die;
		($G) -> ?ability_die;
		($Y) -> ?proficiency_die;
		($B) -> ?setback_die;
		($P) -> ?difficulty_die;
		($R) -> ?challenge_die;
		($F) -> ?force_die;
		(C) -> C
	end, In).

