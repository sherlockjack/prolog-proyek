:- use_module(library(http/html_write)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- dynamic item/2.
:- dynamic character/5.

% character(Name, Atk, Def, Health, Items)
character('Frieren', 100, 5, 1000, [
    item('HealthPotion', increase(health(20))),
    item('PowerCrystal', increase(atk(5)))
]).

character('Serie', 100, 5, 1000, [
    item('HealthPotion', increase(health(20))),
    item('PowerCrystal', increase(atk(5))),
    item('PowerCrystal', increase(atk(5)))
]).

player('Frieren').
enemy('Serie').

player_attack --> {player(PlayerName), enemy(EnemyName)}, html(\attack(PlayerName, EnemyName)).
enemy_attack --> {player(PlayerName), enemy(EnemyName)}, html(\attack(EnemyName, PlayerName)).

attack(AttackerName, DefenderName) -->
  {character(AttackerName, AttackerAtk, _, _, _),
  character(DefenderName, DefenderAtk, DefenderDef, DefenderHealth, DefenderItems),
  Damage is max(AttackerAtk - DefenderDef, 0),
  NewHealth is DefenderHealth - Damage,
  ( NewHealth > 0 ->
    retractall(character(DefenderName, _, _, _, _)),
    assert(character(DefenderName, DefenderAtk, DefenderDef, NewHealth, DefenderItems)),
    swritef(S, '%w attacks %w for %d damage. %w has %d health remaining.\n', 
            [AttackerName, DefenderName, Damage, DefenderName, NewHealth])
    ;
    swritef(S, '%w attacks %w for %d damage. %w is defeated!\n', 
            [AttackerName, DefenderName, Damage, DefenderName])
  )},
  html(p([S])).

use_item(CharName, ItemName) -->
    {character(CharName, CharAtk, CharDef, CharHealth, CharItems),
    member(item(ItemName, Effect), CharItems),
    select(item(ItemName, Effect), CharItems, RemainingItems),

    (Effect = increase(atk(Value)) ->
        NewAtk is CharAtk + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, NewAtk, CharDef, CharHealth, RemainingItems)),
        swritef(S, '%w used %w. Attack increased to %d.\n', [CharName, ItemName, NewAtk])
    ; Effect = increase(def(Value)) ->
        NewDef is CharDef + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(character(CharName, CharAtk, NewDef, CharHealth, RemainingItems))),
        swritef(S, '%w used %w. Defense increased to %d.\n', [CharName, ItemName, NewDef])
    ; Effect = increase(health(Value)) ->
        NewHealth is CharHealth + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharAtk, CharDef, NewHealth, RemainingItems)),
        swritef(S, '%w used %w. Health increased to %d.\n', [CharName, ItemName, NewHealth])
    )},
    html(p([S])).

enemy_action -->
    {enemy(EnemyName),
    character(EnemyName, _, _, _, Items),
    (   member(item(ItemName, increase(atk(_))), Items)
    ->  Action = use_item(EnemyName, ItemName)
    ;   member(item(ItemName, increase(def(_))), Items)
    ->  Action = use_item(EnemyName, ItemName)
    ;   member(item(ItemName, increase(health(_))), Items)
    ->  Action = use_item(EnemyName, ItemName)
    ;   Action = enemy_attack
    )},
    html(\Action).

choose_1 -->
    html([
        h2('Your Turn'),
        \player_attack,
        h2('Enemy Turn'),
        \enemy_action
    ]).

choose_2(ItemName) -->
    {player(PlayerName)},
    html([
        h2('Your Turn'),
        \use_item(PlayerName, ItemName), 
        h2('Enemy Turn'),
        \enemy_action
    ]).