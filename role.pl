:- consult('enemy_logic.pl'). 
:- dynamic character/6.
:- dynamic cooldown/2.
:- dynamic player/1.
:- dynamic use_skill_temp/2.
:- dynamic skill_active/2.
:- dynamic temp_attack/2.
:- dynamic enemy/1.

role(archer, 'penjelasan archer', 3).
role(warrior, 'penjelasan warrior', 2).
role(shielder, 'penjelasan shielder', 3).

initialize_cooldowns :-
    findall(CharName, character(CharName, _, _, _, _, _), CharNames),
    maplist(assert_cooldown, CharNames).

assert_cooldown(CharName) :-
    assert(cooldown(CharName, 0)).

update_cooldowns :-
    findall(CharName, character(CharName, _, _, _, _, _), CharNames),
    maplist(decrement_cooldown, CharNames).

decrement_cooldown(CharName) :-
    cooldown(CharName, Current),
    NewCooldown is max(0, Current - 1),
    retractall(cooldown(CharName, _)),
    assert(cooldown(CharName, NewCooldown)).

get_other_name(CharName, OtherName) :-
    player(CharName) -> enemy(OtherName);player(OtherName).

use_skill(archer, CharName) :-
    get_other_name(CharName, OtherName),
    attack(CharName, OtherName),
    temp_attack(CharName, Output),
    retract(temp_attack(CharName, Output)),
    assert(use_skill_temp(CharName, html([p([CharName, ' activated their skill! I am speed!']), p(Output), p([CharName, ' temporarily decreased their Defense by 50% for 1 turn.'])]))).

use_skill(shielder, CharName) :- 
    assert(use_skill_temp(CharName, html([p([CharName, ' activated their skill!. You can\'t hit me now!']), p([CharName, ' is invulnerable for 1 turn.'])]))).

use_skill(warrior, CharName) :- 
    assert(use_skill_temp(CharName, html([p([CharName, ' activated their skill! This is sparta!']), p([CharName, ' temporarily increased their Attack by 50% for 1 turn.'])]))).

use_skill(CharName) :-
    character(CharName, CharRole, _, _, _, _),
    role(CharRole, _, Cooldown),
    use_skill(CharRole, CharName),
    retractall(skill_active(CharName, _)),
    assert(skill_active(CharName, true)),
    retractall(cooldown(CharName, _)),
    assert(cooldown(CharName, Cooldown)).

use_skill_html(CharName) --> 
    {use_skill_temp(CharName, Output), retract(use_skill_temp(CharName, Output))},
    html(p(Output)).
