-module(ssrpg_roller).

-include("ssrpg_roller_dice.hrl").

-define(opt_specs, [
	{monty, $m, "monty", {integer, undefined}, "Use monty carlo rather than generated roll chances, even if the roll string is short. Overrides -c and -o"},
	{calc, $c, "calc", undefined, "Use explicite generated roll chances even if hte roll string is long. Overrides -m and -o"},
	{odds, $o, "odds", undefined, "Generate the odds of various roll results instead of just the results of one roll. This will automatically choose to use calc or monty depending on the length of the roll string."},
	{help, $?, "help", undefined, "Show a usage thing"}
]).

-export([main/1]).

main(Opts) ->
	random:seed(now()),
	{ok, {RawOpList, RawRoll}} = getopt:parse(?opt_specs, Opts),
	Roll = fix_pool(lists:flatten(RawRoll)),
	OpList = clean_exclusives(RawOpList),
	maybe_do([
		fun maybe_help/2,
		fun maybe_calc/2,
		fun maybe_monty/2,
		fun maybe_either/2,
		fun maybe_roll/2
	], [OpList, Roll]).

clean_exclusives(RawOpsList) ->
	Rollmode = lists:foldl(fun
		(calc, _Mode) ->
			calc;
		({monty, undefined}, Mode) ->
			Mode;
		({monty, _} = M, _) ->
			M;
		(odds, _) ->
			odds;
		(_, Mode) ->
			Mode
	end, undefined, RawOpsList),
	FilteredOps = lists:filter(fun
		(calc) -> false;
		({monty, _}) -> false;
		(odds) -> false;
		(_) -> true
	end, RawOpsList),
	[{calc, Rollmode} | FilteredOps].

maybe_do(FunList, Args) ->
	maybe_do(FunList, false, Args).

maybe_do(_FunList, true, _Args) ->
	ok;

maybe_do([], _Whatever, _Args) ->
	help();

maybe_do([Fun | Tail], _, Args) ->
	Res = erlang:apply(Fun, Args),
	maybe_do(Tail, Res, Args).

maybe_help(OpList, _) ->
	case proplists:get_value(help, OpList) of
		undefined ->
			false;
		_ ->
			help(),
			true
	end.

maybe_calc(OpList, Roll) ->
	case proplists:get_value(calc, OpList) of
		calc ->
			calc(Roll),
			true;
		_ ->
			false
	end.

maybe_monty(OpList, Roll) ->
	case proplists:get_value(calc, OpList) of
		{monty, N} ->
			monty(Roll, N),
			true;
		_ ->
			false
	end.

maybe_either(OpList, Roll) ->
	case proplists:get_value(calc, OpList) of
		odds ->
			if
				length(Roll) >= 6 ->
					monty(Roll, 1000000);
				true ->
					calc(Roll)
			end,
			true;
		_ ->
			false
	end.

maybe_roll(OpList, Roll) ->
	case proplists:get_value(calc, OpList) of
		undefined ->
			roll(Roll),
			true;
		_ ->
			false
	end.

monty(Roll, Iters) ->
	FullRes = lists:map(fun(_) ->
		r:roll(Roll)
	end, lists:seq(1, Iters)),
	output_odds(FullRes).

calc(Roll) ->
	FullRes = lists:map(fun(R) ->
		r:from_list(Roll, R)
	end, g:all_possible(Roll)),
	output_odds(FullRes).

roll(Roll) ->
	Res = r:roll(Roll),
	Str = Res:to_string(),
	io:format("~s~n", [Str]).

output_odds(FullResList) ->
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
	end, Props).

help() ->
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
"and 1 setback die.~n").

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

