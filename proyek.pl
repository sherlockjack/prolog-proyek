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

player_attack :- player(PlayerName), enemy(EnemyName), attack(PlayerName, EnemyName).
enemy_attack :- player(PlayerName), enemy(EnemyName), attack(EnemyName, PlayerName).

attack(AttackerName, DefenderName) :-
  character(AttackerName, AttackerAtk, _, _, _),
  character(DefenderName, DefenderAtk, DefenderDef, DefenderHealth, DefenderItems),
  Damage is max(AttackerAtk - DefenderDef, 0),
  NewHealth is DefenderHealth - Damage,
  ( NewHealth > 0 ->
    retractall(character(DefenderName, _, _, _, _)),
    assert(character(DefenderName, DefenderAtk, DefenderDef, NewHealth, DefenderItems)),
    writef('%w attacks %w for %d damage. %w has %d health remaining.\n', 
            [AttackerName, DefenderName, Damage, DefenderName, NewHealth])
    ;
    writef('%w attacks %w for %d damage. %w is defeated!\n', 
            [AttackerName, DefenderName, Damage, DefenderName])
  ).

use_item(CharName, ItemName) :-
    character(CharName, CharAtk, CharDef, CharHealth, CharItems),
    member(item(ItemName, Effect), CharItems),
    select(item(ItemName, Effect), CharItems, RemainingItems),

    (Effect = increase(atk(Value)) ->
        NewAtk is CharAtk + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, NewAtk, CharDef, CharHealth, RemainingItems)),
        writef('%w used %w. Attack increased to %d.\n', [CharName, ItemName, NewAtk])
    ; Effect = increase(def(Value)) ->
        NewDef is CharDef + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(character(CharName, CharAtk, NewDef, CharHealth, RemainingItems))),
        writef('%w used %w. Defense increased to %d.\n', [CharName, ItemName, NewDef])
    ; Effect = increase(health(Value)) ->
        NewHealth is CharHealth + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharAtk, CharDef, NewHealth, RemainingItems)),
        writef('%w used %w. Health increased to %d.\n', [CharName, ItemName, NewHealth])
    ).

enemy_action :-
    enemy(EnemyName),
    character(EnemyName, _, _, _, Items),
  ( member(item(ItemName, increase(atk(_))), Items) ->
      /* Use atk item */
      use_item(EnemyName, ItemName), !
  ; member(item(ItemName, increase(def(_))), Items) ->
      /* Use def item */
      use_item(EnemyName, ItemName), !
  ; member(item(ItemName, increase(health(_))), Items) ->
      /* Use heal item */
      use_item(EnemyName, ItemName), !
  ; enemy_attack /* Attack if no items left */
  ).

print_items([]).
print_items([item(Name, Effect)|CharItems]) :-
    writef('- Name: %w, Effect: %w\n', [Name, Effect]),
    print_items(CharItems).

show_stats(CharName) :-
    character(CharName, CharAtk, CharDef, CharHealth, CharItems),
    writef('%w stats\n', [CharName]),
    writef('Atk: %d\n', [CharAtk]),
    writef('Def: %d\n', [CharDef]),
    writef('Health: %d\n', [CharHealth]),
    writef('Items:\n', []),
    print_items(CharItems),
    nl.

show_all_stats :-
    player(PlayerName),
    show_stats(PlayerName),
    enemy(EnemyName),
    show_stats(EnemyName).

start :- 
    write('Welcome!\n'),
    write('Here are your and your enemy stats:\n'),

    show_all_stats,

    write('You have two choices: \n'),
    write('Type choose_1. if you want to attack, or choose_2. if you want to use an item').

choose_1 :-
    player_attack,
    enemy_action, 
    show_all_stats.

choose_2 :-
    write('Choose an item name\n'),
    read(ItemName),
    player(PlayerName),
    use_item(PlayerName, ItemName),
    show_all_stats.