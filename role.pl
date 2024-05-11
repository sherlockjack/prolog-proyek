:- dynamic character/6.
:- dynamic role/2.
:- dynamic cooldown/2.

role(archer, 3).
role(warrior, 2).
role(shielder, 3).
role(mage, 3).
role(healer, 2).

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

use_skill(CharName) :-
    character(CharName, CharRole, _, _, _, _),
    role(CharRole, Cooldown),
    retractall(cooldown(CharName, _)),
    assert(cooldown(CharName, Cooldown)).

