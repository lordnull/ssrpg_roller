-define(success, $s).
-define(advantage, $a).
-define(triumph, $h).

-define(failure, $f).
-define(threat, $t).
-define(despair, $d).

-define(light_point, $l).
-define(dark_point, $m).

-define(boost_die, $u).
-define(ability_die, $g).
-define(proficiency_die, $y).

-define(setback_die, $b).
-define(difficulty_die, $p).
-define(challenge_die, $r).

-define(force_die, $w).

-define(boost_sides, [[], [], [?advantage], [?success], [?advantage, ?success], [?advantage, ?advantage]]).
-define(ability_sides, [[], [?success, ?advantage], [?success], [?success], [?success, ?success], [?advantage, ?advantage], [?advantage], [?advantage]]).
-define(proficiency_sides, [[], [?triumph, ?success], [?success], [?advantage, ?advantage], [?success, ?advantage], [?success, ?advantage], [?success, ?success], [?success], [?success, ?success], [?advantage], [?advantage, ?success], [?advantage, ?advantage]]).

-define(setback_sides, [[], [], [?threat], [?threat], [?failure], [?failure]]).
-define(difficulty_sides, [[], [?failure], [?failure, ?failure], [?threat], [?threat], [?threat, ?threat], [?failure, ?threat], [?threat]]).
-define(challenge_sides, [[], [?threat], [?failure], [?threat, ?threat], [?threat, ?failure], [?failure, ?failure], [?despair, ?failure], [?threat], [?failure, ?failure], [?failure, ?threat], [?threat, ?threat], [?failure]]).

-define(force_sides, [[?light_point], [?dark_point, ?dark_point], [?dark_point], [?dark_point], [?light_point, ?light_point], [?dark_point], [?light_point], [?light_point, ?light_point], [?dark_point], [?dark_point], [?light_point, ?light_point], [?dark_point]]).

-define(dice_lookup, [
	{?boost_die, ?boost_sides},
	{?ability_die, ?ability_sides},
	{?proficiency_die, ?proficiency_sides},
	{?setback_die, ?setback_sides},
	{?difficulty_die, ?difficulty_sides},
	{?challenge_die, ?challenge_sides},
	{?force_die, ?force_sides}
]).
