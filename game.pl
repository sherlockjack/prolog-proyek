:- consult('item.pl'). 
:- dynamic item/2.
:- dynamic character/5.
:- dynamic player/1.
:- dynamic enemy/1.

% character(Name, Atk, Def, Health, Items)
create_player(Name, Atk, Def, Health):- get_all_items(Atk, Def, Health, ItemList),
   assert(character(Name, Atk, Def, Health, ItemList)),
   assert(player(Name)).

create_enemy(Name, Atk, Def, Health) :- get_all_items(Atk, Def, Health, ItemList),
   assert(character(Name, Atk, Def, Health, ItemList)),
   assert(enemy(Name)).

player_attack -->
    {player(PlayerName), enemy(EnemyName)}, 
    html(\attack(PlayerName, EnemyName)),
    {check_health}.

enemy_attack -->
    {player(PlayerName), enemy(EnemyName)}, 
    html(\attack(EnemyName, PlayerName)),
    {check_health}.


attack(AttackerName, DefenderName) -->
  {character(AttackerName, AttackerAtk, _, _, _),
  character(DefenderName, DefenderAtk, DefenderDef, DefenderHealth, DefenderItems),
  Damage is max(AttackerAtk - DefenderDef, 0),
  NewHealth is DefenderHealth - Damage,
  ( NewHealth > 0 ->
    retractall(character(DefenderName, _, _, _, _)),
    assert(character(DefenderName, DefenderAtk, DefenderDef, NewHealth, DefenderItems)),
    swritef(Output, '%w attacks %w for %d damage. %w has %d health remaining.\n', 
            [AttackerName, DefenderName, Damage, DefenderName, NewHealth])
    ;
    swritef(Output, '%w attacks %w for %d damage. %w is defeated!\n', 
            [AttackerName, DefenderName, Damage, DefenderName])
  )},
  html(p(Output)).

use_item(CharName, ItemName) -->
    {character(CharName, CharAtk, CharDef, CharHealth, CharItems),
    member(item(ItemName, Effect), CharItems),
    select(item(ItemName, Effect), CharItems, RemainingItems),

    (Effect = increase('Attack', Value) ->
        NewAtk is CharAtk + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, NewAtk, CharDef, CharHealth, RemainingItems)),
        swritef(Output, '%w used %w. Attack increased to %d.\n', [CharName, ItemName, NewAtk])
    ; Effect = increase('Defense', Value) ->
        NewDef is CharDef + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharAtk, NewDef, CharHealth, RemainingItems)),
        swritef(Output, '%w used %w. Defense increased to %d.\n', [CharName, ItemName, NewDef])
    ; Effect = increase('Health', Value) ->
        NewHealth is CharHealth + Value,
        retractall(character(CharName, _, _, _, _)),
        assert(character(CharName, CharAtk, CharDef, NewHealth, RemainingItems)),
        swritef(Output, '%w used %w. Health increased to %d.\n', [CharName, ItemName, NewHealth])
    )},
    html(p(Output)).

calc_power(PlayerAtk, PlayerDef, PlayerHealth, EnemyAtk, EnemyDef, EnemyHealth, Result) :-
    TrueEnemyAtk is EnemyAtk-PlayerDef,
    TruePlayerAtk is PlayerAtk-EnemyDef,
    Offset is -min(TruePlayerAtk, TrueEnemyAtk) + 1,
    Result is (EnemyHealth / (TruePlayerAtk+Offset)) / (PlayerHealth / (TrueEnemyAtk+Offset)).

predict_power(Type, Value, NewPower) :-
    player(PlayerName),
    character(PlayerName, PlayerAtk, PlayerDef, PlayerHealth, _),
    enemy(EnemyName),
    character(EnemyName, EnemyAtk, EnemyDef, EnemyHealth, _),

    ( Type == 'Attack' -> 
        calc_power(PlayerAtk, PlayerDef, PlayerHealth, EnemyAtk+Value, EnemyDef, EnemyHealth, NewPower)
    ; Type == 'Defense' -> 
        calc_power(PlayerAtk, PlayerDef, PlayerHealth, EnemyAtk, EnemyDef+Value, EnemyHealth, NewPower)
    ; Type == 'Health' -> 
        calc_power(PlayerAtk, PlayerDef, PlayerHealth, EnemyAtk, EnemyDef, EnemyHealth+Value, NewPower)
    ).

best_item_finder([], BestItemName, BestItemPower) :-
    BestItemName = '',
    BestItemPower = -1.

best_item_finder([item_power(ItemName, ItemPower) | Rest], BestItemName, BestItemPower) :-
    best_item_finder(Rest, PrevItemName, PrevItemPower),
    (
        ItemPower > PrevItemPower ->
        BestItemName = ItemName,
        BestItemPower = ItemPower
    ;
        BestItemName = PrevItemName,
        BestItemPower = PrevItemPower
    ).

best_item(Items, BestItemName, BestItemPower) :-
    findall(item_power(ItemName, NewPower),
            (member(item(ItemName, increase(Type, Value)), Items), 
            predict_power(Type, Value, NewPower)),
            PowerList),
    
    best_item_finder(PowerList, BestItemName, BestItemPower).

enemy_action -->
    {player(PlayerName),
    character(PlayerName, PlayerAtk, PlayerDef, PlayerHealth, _),
    enemy(EnemyName),
    character(EnemyName, EnemyAtk, EnemyDef, EnemyHealth, EnemyItems),

    NewPlayerHealth is PlayerHealth - (EnemyAtk - PlayerDef),
    (
        NewPlayerHealth =< 0 ->
        Action = enemy_attack
    ; 
        calc_power(PlayerAtk, PlayerDef, NewPlayerHealth, EnemyAtk, EnemyDef, EnemyHealth, Power),

        best_item(EnemyItems, BestItemName, BestItemPower),
        (
            Power > BestItemPower ->
            Action = enemy_attack
        ;
            Action = use_item(EnemyName, BestItemName)
        )
    )},
    html(\Action).

check_health :-
    player(PlayerName),
    enemy(EnemyName),
    character(PlayerName, _, _, PlayerHealth, _),
    character(EnemyName, _, _, EnemyHealth, _),
    (PlayerHealth =< 0 -> assert(game_outcome(lose));
     EnemyHealth =< 0 -> assert(game_outcome(win));
     true).
    

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