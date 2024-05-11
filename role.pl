:- consult('game.pl'). 
:- dynamic role/3.
:- dynamic cooldown/2.

role(archer, double_action, 3).
role(warrior, strong_attack, 2).
role(shielder, high_defense, 3).
role(mage, reflect_attack, 3).
role(healer, heal, 2).

initialize_cooldowns :-
    findall(Name, character(Name, _, _, _, _, _), NameList),
    maplist(assert_cooldown, NameList).

assert_cooldown(Name) :-
    assert(cooldown(Name, 0)).

update_cooldowns :-
    findall(Name, character(Name, _, _, _, _, _), NameList),
    maplist(decrement_cooldown, NameList).

decrement_cooldown(Name) :-
    cooldown(Name, Current),
    NewCooldown is max(0, Current - 1),
    retract(cooldown(Name, _)),
    assert(cooldown(Name, NewCooldown)).
